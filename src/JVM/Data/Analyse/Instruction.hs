{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists #-}

-- | Analyses lists of instructions, inserting StackMapTable attributes where needed & resolving labels.
module JVM.Data.Analyse.Instruction (analyseStackChange, calculateStackMapFrames, insertStackMapTable) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), methodParam)
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type (ClassInfoType (ArrayClassInfoType, ClassInfoType), FieldType (..), PrimitiveType (..), fieldTypeToClassInfoType)

-- | Details how the stack changes between two instructions
data StackDiff
    = -- | Pushes the given types onto the stack
      StackPush (NonEmpty FieldType)
    | -- | Pops the given number of types from the stack
      StackPop Int
    | StackSame
    deriving (Show, Eq, Ord)

stackPop :: Int -> StackDiff
stackPop 0 = StackSame
stackPop n | n < 0 = error "stackPop: negative"
stackPop n = StackPop n

instance Semigroup StackDiff where
    StackSame <> x = x
    x <> StackSame = x
    StackPush ts <> StackPush ts' = StackPush (ts <> ts')
    StackPop n <> StackPop n' = stackPop (n + n')
    StackPush ts <> StackPop n = maybe StackSame StackPush (NE.nonEmpty $ NE.drop n ts)
    StackPop n <> StackPush ts = maybe StackSame StackPush (NE.nonEmpty $ NE.drop n ts)

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
    applyMany diffs x = foldr apply x diffs

instance Apply StackDiff Stack where
    apply (StackPush ts) s = NE.toList ts ++ s
    apply (StackPop n) s = drop n s
    apply StackSame s = s

instance Apply LocalsDiff Locals where
    apply (LocalsPush ts) s = NE.toList ts ++ s
    apply (LocalsPop n) s = drop n s
    apply LocalsSame s = s

analyseStackChange :: (Stack, Locals) -> MethodDescriptor -> Instruction -> Maybe (StackDiff, LocalsDiff)
analyseStackChange _ desc (ALoad idx) = do
    idx' <- desc `methodParam` fromIntegral idx
    pure (StackPush [idx'], LocalsSame)
analyseStackChange (stack : _, locals) _ (AStore idx) = pure (stackPop 1, if length locals < fromIntegral idx then LocalsPush [stack] else LocalsSame)
analyseStackChange ([], _) _ (AStore _) = error "AStore with empty stack"
analyseStackChange _ _ AReturn = pure (stackPop 1, LocalsSame)
analyseStackChange _ _ Return = pure (stackPop 1, LocalsSame)
analyseStackChange _ _ (LDC x) = pure (StackPush [ldcEntryToFieldType x], LocalsSame)
analyseStackChange _ _ AConstNull = pure (StackPush [ObjectFieldType "java/lang/Object"], LocalsSame)
analyseStackChange _ _ (Goto _) = pure (StackSame, LocalsSame)
analyseStackChange _ _ (IfEq _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfNe _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfLt _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfGe _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfGt _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (IfLe _) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (InvokeStatic _ _ (MethodDescriptor args _)) = do
    pure (stackPop (length args - 1), LocalsSame)
analyseStackChange _ _ (InvokeVirtual _ _ (MethodDescriptor args _)) = do
    pure (stackPop (length args), LocalsSame)
analyseStackChange _ _ (InvokeDynamic _ _ (MethodDescriptor args _)) = do
    pure (stackPop (length args), LocalsSame)
analyseStackChange _ _ (InvokeInterface _ _ (MethodDescriptor args _)) = do
    pure (stackPop (length args), LocalsSame)
analyseStackChange _ _ (PutStatic{}) = pure (StackPop 1, LocalsSame)
analyseStackChange _ _ (GetStatic _ _ ft) = pure (StackPush [ft], LocalsSame)
analyseStackChange _ _ (CheckCast _) = pure (StackSame, LocalsSame)
analyseStackChange _ _ (Label _) = pure (StackSame, LocalsSame)

-- analyseStackChange _ _ other = error ("Not implemented: " ++ show other)

-- | Analyses a list of instructions, returning the stack and locals at each point.
analyseStackMapTable :: MethodDescriptor -> [Instruction] -> (Stack, Locals, [(StackDiff, LocalsDiff)])
analyseStackMapTable desc = go ([], [])
  where
    go :: (Stack, Locals) -> [Instruction] -> (Stack, Locals, [(StackDiff, LocalsDiff)])
    go (x, l) [] = (x, l, [])
    go (stack, locals) (i : is) =
        case analyseStackChange (stack, locals) desc i of
            Nothing -> go (stack, locals) is
            Just (stackDiff, localsDiff) ->
                let (s, l, diffs) = go (apply stackDiff stack, apply localsDiff locals) is
                 in (s, l, (stackDiff, localsDiff) : diffs)

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
        jumps = findJumps code
        jumpDiffs = fmap (findJumpDiff desc code) jumps
        frames = zipWith (\a (s, l, d) -> calculateStackMapFrame (s, l) a d) (snd <$> jumps) jumpDiffs
     in
        frames

calculateStackMapFrame :: (Stack, Locals) -> Label -> (StackDiff, LocalsDiff) -> StackMapFrame
calculateStackMapFrame _ target (StackSame, LocalsSame) = SameFrame target
calculateStackMapFrame _ target (StackSame, LocalsPush xs) = AppendFrame (NE.toList $ fieldTypeToVerificationType <$> xs) target
calculateStackMapFrame _ target (StackSame, LocalsPop n) = ChopFrame (fromIntegral n) target
calculateStackMapFrame _ target (StackPush xs, LocalsSame) = SameLocals1StackItemFrame (fieldTypeToVerificationType (NE.last xs)) target
calculateStackMapFrame _ target (StackPush xs, LocalsPush ys) = FullFrame (NE.toList $ fieldTypeToVerificationType <$> xs) (NE.toList $ fieldTypeToVerificationType <$> ys) target
calculateStackMapFrame (stack, locals) _ (x, y) = FullFrame (NE.toList $ fieldTypeToVerificationType <$> NE.fromList stack) (NE.toList $ fieldTypeToVerificationType <$> NE.fromList locals) (Label 0)

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

findJumpDiff :: MethodDescriptor -> [Instruction] -> (Integer, Label) -> (Stack, Locals, (StackDiff, LocalsDiff))
findJumpDiff desc code (jump, label) =
    let slice = takeWhile (/= Label label) (drop (fromIntegral jump) code)
        (stack, locals, diffs) = analyseStackMapTable desc slice
     in (stack, locals, mconcat diffs)

{- | Finds all the instructions in which a jump occurs and the instruction to which it jumps.
For example, given input @[.., IfEq l, .., Label l, x, ..]@ this will return @[(n,  l)]@ where @n@ is the index of the @IfEq l@ instruction.
-}
findJumps :: [Instruction] -> [(Integer, Label)]
findJumps xs = mapMaybe f (zip xs [0 ..])
  where
    f :: (Instruction, Integer) -> Maybe (Integer, Label)
    f (inst, i) = (i,) <$> jumpTarget inst
