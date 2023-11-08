{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists #-}

-- | Analyses lists of instructions, inserting StackMapTable attributes where needed & resolving labels.
module JVM.Data.Analyse.Instruction (normaliseStackDiff, Apply (..), StackDiff (..), stackPush, stackPop, localsPop, stackPopAndPush, LocalsDiff (..), analyseStackChange, calculateStackMapFrames, analyseStackMapTable, insertStackMapTable, findJumps) where

import Control.Applicative (liftA2)
import Data.List (foldl', genericLength)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Split (splitWhen)
import Data.Maybe (isJust, mapMaybe, maybeToList)
import GHC.Stack (HasCallStack)
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), methodParams, returnDescriptorType)
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type (ClassInfoType (ArrayClassInfoType, ClassInfoType), FieldType (..), PrimitiveType (..), fieldTypeToClassInfoType)
import JVM.Data.Pretty

-- | Details how the stack changes between two instructions
data StackDiff
    = -- | Pushes the given types onto the stack
      StackPush (NonEmpty FieldType)
    | -- | Pops the given number of types from the stack
      StackPop Int
    | -- | Pops the given number of types from the stack, then pushes the given types.
      -- This has some specific (and somewhat weird) semantics that means it must prevent "optimisation".
      -- For example, suppose we wrote @StackPopAndPush 1 [someFieldType]@. You might think this could be optimised to @StackSame@, but no!
      -- The popping refers to the *previous* (imaginary) stack, not the current one. So if the previous stack was @[someOtherFieldType]@, it should become @[someFieldType]@. Optimising to @StackSame@ would mean it remains @[someOtherFieldType]@.
      StackPopAndPush Int (NonEmpty FieldType)
    | -- | The stack is unchanged
      StackSame
    deriving (Show, Eq, Ord)

instance Pretty StackDiff where
    pretty StackSame = "same"
    pretty (StackPush ts) = "push " <> pretty ts
    pretty (StackPopAndPush n ts) = "pop " <> pretty n <> " then push " <> pretty ts
    pretty (StackPop n) = "pop " <> pretty n

stackPop :: HasCallStack => Int -> StackDiff
stackPop n | n < 0 = error "stackPop: negative"
stackPop 0 = StackSame
stackPop n = StackPop n

stackPush :: [FieldType] -> StackDiff
stackPush [] = StackSame
stackPush ts = StackPush (NE.fromList ts)

stackPopAndPush :: HasCallStack => Int -> [FieldType] -> StackDiff
stackPopAndPush n _ | n < 0 = error "stackPopAndPush: negative"
stackPopAndPush 0 ts = stackPush ts
stackPopAndPush n [] = stackPop n
stackPopAndPush n ts =
    -- We don't perform any optimisations here, see the docs for StackPopAndPush constructor
    StackPopAndPush n (NE.fromList ts)

-- | "Normalises" a stack diff. All this really does is turns 'StackPopAndPush' into 'StackPop' and 'StackPush' where possible, undoing the special semantics of 'StackPopAndPush'.
normaliseStackDiff :: StackDiff -> StackDiff
normaliseStackDiff (StackPopAndPush n ts) = case n `compare` length ts of
    LT -> stackPush (NE.drop n ts)
    EQ -> StackSame
    GT -> stackPop (n - length ts)
normaliseStackDiff x = x

instance Semigroup StackDiff where
    StackSame <> x = x
    x <> StackSame = x
    StackPush ts <> StackPush ts' = StackPush (ts <> ts')
    StackPop n <> StackPop n' = stackPop (n + n')
    -- \^^ trivial cases
    StackPush ts <> StackPop n = case n `compare` length ts of
        LT -> stackPush (NE.drop n ts)
        EQ -> StackSame
        GT -> stackPop (n - length ts)
    StackPop n <> StackPush ts = case n `compare` length ts of
        LT -> stackPush (NE.drop n ts)
        EQ -> StackSame
        GT -> stackPop (n - length ts)
    StackPop n <> StackPopAndPush n' ts = stackPopAndPush (n + n') (NE.toList ts)
    StackPopAndPush n ts <> StackPop n' = (stackPop n <> StackPush ts) <> stackPop n'
    StackPopAndPush n ts <> StackPopAndPush n' ts' = stackPop n <> StackPush ts <> StackPopAndPush n' ts'
    StackPush ts <> StackPopAndPush n ts' = (stackPush (NE.toList ts) <> stackPop n) <> StackPush ts'
    StackPopAndPush n ts <> StackPush ts' = (stackPop n <> StackPush ts) <> StackPush ts'

instance Monoid StackDiff where
    mempty = StackSame

type Stack = [FieldType]

-- | Details how the local variables change between two instructions
data LocalsDiff
    = -- | Pushes the given types onto the locals
      LocalsPush (NonEmpty FieldType)
    | -- | Pops the given number of types from the locals
      LocalsPop Int
    | LocalsSame
    deriving (Show, Eq, Ord)

instance Pretty LocalsDiff where
    pretty LocalsSame = "same"
    pretty (LocalsPush ts) = "push " <> pretty ts
    pretty (LocalsPop n) = "pop " <> pretty n

localsPop :: Int -> LocalsDiff
localsPop 0 = LocalsSame
localsPop n | n < 0 = error "localsPop: negative"
localsPop n = LocalsPop n

instance Semigroup LocalsDiff where
    LocalsSame <> x = x
    x <> LocalsSame = x
    LocalsPush ts <> LocalsPush ts' = LocalsPush (ts <> ts')
    LocalsPop n <> LocalsPop n' = localsPop (n + n')
    LocalsPush ts <> LocalsPop n = maybe LocalsSame LocalsPush (NE.nonEmpty $ NE.drop n ts)
    LocalsPop n <> LocalsPush ts = maybe LocalsSame LocalsPush (NE.nonEmpty $ NE.drop n ts)

instance Monoid LocalsDiff where
    mempty = LocalsSame

type Locals = [FieldType]

class Apply diff a | diff -> a where
    apply :: diff -> a -> a
    applyMany :: [diff] -> a -> a
    applyMany diffs x = foldl' (flip apply) x diffs

instance Apply (Maybe StackDiff) Stack where
    apply (Just diff) = apply diff
    apply Nothing = id

instance Apply (Maybe LocalsDiff) Locals where
    apply (Just diff) = apply diff
    apply Nothing = id

instance Apply StackDiff Stack where
    apply (StackPush ts) s = NE.toList ts ++ s
    apply (StackPop n) s = drop n s
    apply StackSame s = s
    apply (StackPopAndPush n ts) s = applyMany [stackPop n, StackPush ts] s

instance Apply LocalsDiff Locals where
    apply (LocalsPush ts) s = s ++ NE.toList ts
    apply (LocalsPop n) s = drop n s
    apply LocalsSame s = s

analyseStackChange :: HasCallStack => (Stack, Locals) -> MethodDescriptor -> Instruction -> Maybe (StackDiff, LocalsDiff)
analyseStackChange (_, locals) desc (ALoad idx) = do
    let (!!?) :: [a] -> Int -> Maybe a
        _ !!? n | n < 0 = Nothing
        [] !!? _ = Nothing
        (x : _) !!? 0 = Just x
        (_ : xs) !!? n = xs !!? (n - 1)

    idx' <- locals !!? fromIntegral idx
    pure (stackPush [idx'], LocalsSame)
analyseStackChange (stack : _, locals) desc (AStore idx) =
    pure
        ( stackPop 1
        , if idx >= genericLength (methodParams desc) && length locals <= fromIntegral idx
            then LocalsPush [stack]
            else LocalsSame
        )
analyseStackChange ([], _) _ (AStore x) = error ("AStore with empty stack: " <> show x)
analyseStackChange _ _ AReturn = pure (stackPop 1, LocalsSame)
analyseStackChange _ _ Return = pure (stackPop 1, LocalsSame)
analyseStackChange _ _ (LDC x) = pure (StackPush [ldcEntryToFieldType x], LocalsSame)
analyseStackChange _ _ AConstNull = pure (StackPush [ObjectFieldType "java/lang/Object"], LocalsSame)
analyseStackChange _ _ (Goto _) = pure (StackSame, LocalsSame)
analyseStackChange (stack : _, _) _ Dup = pure (StackPush [stack], LocalsSame)
analyseStackChange ([], _) _ Dup = error "Dup with empty stack"
analyseStackChange _ _ (IfEq _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfNe _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfLt _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfGe _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfGt _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfLe _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (InvokeStatic _ _ (MethodDescriptor args ret)) = pure (stackPopAndPush (length args) (maybeToList $ returnDescriptorType ret), LocalsSame)
analyseStackChange _ _ (InvokeVirtual _ _ (MethodDescriptor args ret)) = pure (stackPopAndPush (length args + 1) (maybeToList $ returnDescriptorType ret), LocalsSame)
analyseStackChange _ _ (InvokeDynamic _ _ (MethodDescriptor args ret)) = pure (stackPopAndPush (length args - 1) (maybeToList $ returnDescriptorType ret), LocalsSame)
analyseStackChange _ _ (InvokeInterface _ _ (MethodDescriptor args ret)) = pure (stackPopAndPush (length args - 1) (maybeToList $ returnDescriptorType ret), LocalsSame)
analyseStackChange _ _ (PutStatic{}) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ f@(GetField _ _ ft) = pure (stackPopAndPush 1 [ft], LocalsSame)
analyseStackChange _ _ (GetStatic _ _ ft) = pure (StackPush [ft], LocalsSame)
analyseStackChange _ _ (CheckCast _) = pure (StackSame, LocalsSame)
analyseStackChange _ _ (Label _) = Nothing

-- | Analyses a list of instructions, returning the stack and locals at each point.
analyseStackMapTable :: HasCallStack => MethodDescriptor -> [Instruction] -> (Stack, Locals, [Maybe (StackDiff, LocalsDiff)])
analyseStackMapTable desc = go ([], methodParams desc)
  where
    go :: HasCallStack => (Stack, Locals) -> [Instruction] -> (Stack, Locals, [Maybe (StackDiff, LocalsDiff)])
    go (x, l) [] = (x, l, [])
    go (stack, locals) (i : is) =
        case analyseStackChange (stack, locals) desc i of
            Nothing -> let (s, l, xs) = go (stack, locals) is in (s, l, Nothing : xs)
            Just (stackDiff, localsDiff) ->
                let (s, l, diffs) = go (apply stackDiff stack, apply localsDiff locals) is
                 in (s, l, Just (stackDiff, localsDiff) : diffs)

-- | Inserts a StackMapTable entry into the CodeBuilder
insertStackMapTable :: Monad m => MethodDescriptor -> CodeBuilderT m ()
insertStackMapTable desc = do
    -- The process here is fairly simple:
    -- 1. Analyse the instructions, getting the stack and locals diffs at each point
    -- 2. Find all the instructions in which a jump occurs
    -- 3. Find the diffs between each jump source and jump target
    -- 4. Insert a StackMapTable entry for each jump source, with the diffs from the jump source to the jump target
    code <- getCode
    mapM_ appendStackMapFrame (calculateStackMapFrames desc code)

calculateStackMapFrames :: MethodDescriptor -> [Instruction] -> [StackMapFrame]
calculateStackMapFrames desc code =
    let
        (_, _, frameDiffs) = analyseStackMapTable desc code

        isLabel (Label x) = Just x
        isLabel _ = Nothing

        labels = mapMaybe isLabel code
        jumps = fmap mconcat (fmap snd <$> splitWhen (isJust . isLabel . fst) (zip code frameDiffs))

        -- jumps = [[ALoad 1], []]
        jumpsAndLabels = zip (Nothing : (Just <$> reverse labels)) jumps
        x =
            foldl'
                ( flip $ \(label, res) (acc, stack, locals) ->
                    let
                        stack' = apply (fmap fst res) stack
                        locals' = apply (fmap snd res) locals
                     in
                        case liftA2 (,) label res of
                            Just (label', res') ->
                                ( calculateStackMapFrame (stack', locals') label' res' : acc
                                , stack'
                                , locals'
                                )
                            _ -> (acc, stack', locals')
                )
                ([], [], methodParams desc)
                jumpsAndLabels
     in
        (\(a, _, _) -> a) x

-- calculateFrame :: MethodDescriptor -> (StackDiff, LocalsDiff) -> [Instruction] -> [(Label, StackDiff, LocalsDiff)]
-- calculateFrame desc prev code = go prev code []
--   where
--     go :: (StackDiff, LocalsDiff) -> [Instruction] -> [Instruction] -> [(Label, StackDiff, LocalsDiff)]
--     go _ [] _ = [] -- if we run out of instructions before seeing another label, there's no need for a frame
--     go prev ((Label label) : xs) acc = do
--         let (newStack, newLocals, diffs) = analyseStackMapTable desc acc
--             newDiffs@(newDiffs1, newDiffs2) = (prev <> mconcat diffs)
--          in (label, newDiffs1, newDiffs2) : go newDiffs xs []
--     go prev (x : xs) acc = go prev xs (acc ++ [x])

calculateStackMapFrame :: (Stack, Locals) -> Label -> (StackDiff, LocalsDiff) -> StackMapFrame
calculateStackMapFrame _ target (StackSame, LocalsSame) = SameFrame target
calculateStackMapFrame _ target (StackSame, LocalsPush xs) = AppendFrame (NE.toList $ fieldTypeToVerificationType <$> xs) target
calculateStackMapFrame _ target (StackSame, LocalsPop n) = ChopFrame (fromIntegral n) target
calculateStackMapFrame _ target (StackPush xs, LocalsSame) = SameLocals1StackItemFrame (fieldTypeToVerificationType (NE.last xs)) target
calculateStackMapFrame _ target (StackPush xs, LocalsPush ys) = FullFrame (NE.toList $ fieldTypeToVerificationType <$> xs) (NE.toList $ fieldTypeToVerificationType <$> ys) target
calculateStackMapFrame (stack, locals) target _ = FullFrame (fieldTypeToVerificationType <$> stack) (fieldTypeToVerificationType <$> locals) target

fieldTypeToVerificationType :: FieldType -> VerificationTypeInfo
fieldTypeToVerificationType (ObjectFieldType x) = ObjectVariableInfo (ClassInfoType x)
fieldTypeToVerificationType (ArrayFieldType x) = ObjectVariableInfo (ArrayClassInfoType (fieldTypeToClassInfoType x))
fieldTypeToVerificationType (PrimitiveFieldType Byte) = IntegerVariableInfo
fieldTypeToVerificationType (PrimitiveFieldType Char) = IntegerVariableInfo
fieldTypeToVerificationType (PrimitiveFieldType Double) = DoubleVariableInfo
fieldTypeToVerificationType (PrimitiveFieldType Float) = FloatVariableInfo
fieldTypeToVerificationType (PrimitiveFieldType Int) = IntegerVariableInfo
fieldTypeToVerificationType (PrimitiveFieldType Long) = LongVariableInfo
fieldTypeToVerificationType (PrimitiveFieldType Short) = IntegerVariableInfo
fieldTypeToVerificationType (PrimitiveFieldType Boolean) = IntegerVariableInfo

{- | Finds the difference between the stack and locals at the given jump source and jump target.
@findJumpDiff desc code (jumpSourceIdx, label)@ takes all the instructions between @jumpSourceIdx@ and @label@ in @code@
and analyses their stack & locals changes, returning the stack and locals at @jumpSourceIdx@ and the diffs between @jumpSourceIdx@ and @label@.
-}

-- findJumpDiff :: MethodDescriptor -> [Instruction] -> (Integer, Label) -> (Stack, Locals, (StackDiff, LocalsDiff))
-- findJumpDiff desc code (jump, label) =
--     let slice = takeWhile (/= Label label) (drop (fromIntegral jump) code)
--         (stack, locals, diffs) = analyseStackMapTable desc slice
--      in (stack, locals, mconcat diffs)

{- | Finds all the instructions in which a jump occurs and the instruction to which it jumps.
For example, given input @[.., IfEq l, .., Label l, x, ..]@ this will return @[(n,  l)]@ where @n@ is the index of the @IfEq l@ instruction.
-}
findJumps :: [Instruction] -> [(Integer, Label)]
findJumps xs = mapMaybe f (zip xs [0 ..])
  where
    f :: (Instruction, Integer) -> Maybe (Integer, Label)
    f (inst, i) = (i,) <$> jumpTarget inst
