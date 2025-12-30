{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

{- | Generate a stack map table for a method.
This process MUST run last in the high level stage -
modifications to the code after this point will invalidate the stack map table and cause invalid class files to be generated.
-}
module JVM.Data.Analyse.StackMap where

import Control.Lens.Fold
import Data.Generics.Sum (AsAny (_As))
import Data.List
import Data.Maybe (fromJust, maybeToList)
import Effectful (Eff, runPureEff)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.State.Static.Local (State, execState, get, gets, modify, put)
import GHC.Stack (HasCallStack)
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), returnDescriptorType)
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type (FieldType (..), PrimitiveType (..), classInfoTypeToFieldType, fieldTypeToClassInfoType)
import JVM.Data.Pretty (Pretty (pretty), showPretty)
import JVM.Data.Raw.Types
import Prettyprinter (vsep)

data BasicBlock = BasicBlock
    { index :: Int
    , instructions :: [Instruction]
    , end :: Maybe Label
    }
    deriving (Show, Eq)

data Frame = Frame
    { locals :: [LocalVariable]
    , stack :: [StackEntry]
    }
    deriving (Show, Eq)

data LocalVariable = Uninitialised | LocalVariable FieldType
    deriving (Show, Eq)

instance Pretty LocalVariable where
    pretty Uninitialised = "uninitialised"
    pretty (LocalVariable ft) = pretty ft

data StackEntry = StackEntry FieldType | StackEntryTop | StackEntryNull
    deriving (Show, Eq)

instance Pretty StackEntry where
    pretty StackEntryTop = "top"
    pretty StackEntryNull = "null"
    pretty (StackEntry ft) = pretty ft

lvToStackEntry :: LocalVariable -> StackEntry
lvToStackEntry Uninitialised = StackEntryTop
lvToStackEntry (LocalVariable ft) = StackEntry ft

stackEntryToLV :: StackEntry -> LocalVariable
stackEntryToLV StackEntryTop = Uninitialised
stackEntryToLV StackEntryNull = Uninitialised
stackEntryToLV (StackEntry ft) = LocalVariable ft

splitIntoBasicBlocks :: [Instruction] -> [BasicBlock]
splitIntoBasicBlocks [] = []
splitIntoBasicBlocks l =
    let blockToInstAndLabel = splitOnLabels l
     in zipWith (\i (l, b) -> BasicBlock i b l) [0 ..] blockToInstAndLabel

splitOnLabels :: [Instruction] -> [(Maybe Label, [Instruction])]
splitOnLabels xs = go xs []
  where
    go :: [Instruction] -> [Instruction] -> [(Maybe Label, [Instruction])]
    go [] acc = [(Nothing, acc)]
    go (x : xs) acc = case x ^? _As @"Label" of
        Just l' -> (Just l', acc) : go xs []
        Nothing -> go xs (acc <> [x])

topFrame :: MethodDescriptor -> Frame
topFrame (MethodDescriptor args _) = Frame (map LocalVariable args) []

analyseBlockDiff :: (HasCallStack) => Frame -> BasicBlock -> Frame
analyseBlockDiff current block = foldl' (flip analyseInstruction) current (takeWhileInclusive (not . isConditionalJump) block.instructions)
  where
    isConditionalJump :: Instruction -> Bool
    isConditionalJump (IfEq _) = True
    isConditionalJump (IfNe _) = True
    isConditionalJump (IfLt _) = True
    isConditionalJump (IfGe _) = True
    isConditionalJump (IfGt _) = True
    isConditionalJump (IfLe _) = True
    isConditionalJump _ = False

    analyseInstruction :: (HasCallStack) => Instruction -> Frame -> Frame
    analyseInstruction inst frame =
        runPureEff $
            runReader block $
                execState frame $
                    analyse inst
      where
        analyse :: Instruction -> Analyser
        analyse = \case
            (Label _) -> error "Label should not be encountered in analyseInstruction"
            (ALoad i) -> loads "ALoad" i
            (ILoad i) -> loads "ILoad" i
            (AStore i) -> stores (fromIntegral i)
            (IStore i) -> stores (fromIntegral i)
            AReturn -> pops 1
            AConstNull -> pushesEntry StackEntryNull
            Return -> pure ()
            LDC t -> pushes (ldcEntryToFieldType t)
            Dup -> do
                s <- gets (.stack)
                case s of
                    [] -> error "Stack underflow during dup"
                    head : _ -> pushesEntry head
            IfEq _ -> pops 1
            IfNe _ -> pops 1
            IfLt _ -> pops 1
            IfGe _ -> pops 1
            IfGt _ -> pops 1
            IfLe _ -> pops 1
            CheckCast ft -> replaceTop (classInfoTypeToFieldType ft)
            Instanceof _ -> replaceTop (PrimitiveFieldType Int)
            InvokeStatic _ _ md -> do
                pops (length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeVirtual _ _ md -> do
                pops (1 + length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeInterface _ _ md -> do
                pops (1 + length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeDynamic _ _ md -> do
                pops (length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeSpecial _ _ md -> do
                pops (1 + length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            PutStatic{} -> pops 1
            GetField _ _ ft -> do
                pops 1
                pushes ft
            GetStatic _ _ ft -> pushes ft
            PutField{} -> pops 2
            Goto _ -> pure ()
            New t -> pushes (classInfoTypeToFieldType t)

frameDiffToSMF :: (HasCallStack) => Frame -> BasicBlock -> StackMapFrame
frameDiffToSMF f1@(Frame locals1 stack1) bb = do
    let (Frame locals2 stack2) = analyseBlockDiff f1 bb
    if
        | locals1 == locals2 && stack1 == stack2 -> SameFrame (fromJust bb.end)
        | stack1 == stack2 && locals1 `isPrefixOf` locals2 -> AppendFrame (map lvToVerificationTypeInfo (drop (length locals1) locals2)) (fromJust bb.end)
        | [x] <- stack2, locals1 == locals2 -> SameLocals1StackItemFrame (seToVerificationTypeInfo x) (fromJust bb.end)
        | locals1 == locals2 && locals1 `isSuffixOf` locals2 -> ChopFrame (fromIntegral $ length locals1 - length locals2) (fromJust bb.end)
        | otherwise -> FullFrame (map lvToVerificationTypeInfo locals2) (map seToVerificationTypeInfo stack2) (fromJust bb.end)

lvToVerificationTypeInfo :: LocalVariable -> VerificationTypeInfo
lvToVerificationTypeInfo Uninitialised = TopVariableInfo
lvToVerificationTypeInfo (LocalVariable ft) = case ft of
    PrimitiveFieldType Int -> IntegerVariableInfo
    PrimitiveFieldType Float -> FloatVariableInfo
    PrimitiveFieldType Long -> LongVariableInfo
    PrimitiveFieldType Double -> DoubleVariableInfo
    _ -> ObjectVariableInfo (fieldTypeToClassInfoType ft)

seToVerificationTypeInfo :: StackEntry -> VerificationTypeInfo
seToVerificationTypeInfo StackEntryTop = TopVariableInfo
seToVerificationTypeInfo StackEntryNull = NullVariableInfo
seToVerificationTypeInfo (StackEntry ft) = case ft of
    PrimitiveFieldType Int -> IntegerVariableInfo
    PrimitiveFieldType Float -> FloatVariableInfo
    PrimitiveFieldType Long -> LongVariableInfo
    PrimitiveFieldType Double -> DoubleVariableInfo
    _ -> ObjectVariableInfo (fieldTypeToClassInfoType ft)

calculateStackMapFrames :: (HasCallStack) => MethodDescriptor -> [Instruction] -> [StackMapFrame]
calculateStackMapFrames md code = do
    let blocks = splitIntoBasicBlocks code
    let top = topFrame md
    let frames = scanl analyseBlockDiff top blocks

    zipWith frameDiffToSMF frames (init blocks)

replaceAtOrGrow :: Int -> LocalVariable -> [LocalVariable] -> [LocalVariable]
replaceAtOrGrow i x xs
    | i < length xs = replaceAt i x xs
    | otherwise = xs <> replicate (i - length xs) Uninitialised <> [x]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs <> [x] <> drop (i + 1) xs

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs)
    | p x = x : takeWhileInclusive p xs
    | otherwise = [x]

-- | Analyser monad for stack map frame analysis
type Analyser = Eff '[State Frame, Reader BasicBlock] ()

-- | Pops n items off the stack
pops :: Int -> Analyser
pops n = modify $ \f -> f{stack = drop n f.stack}

-- | Pushes a single type onto the stack
pushes :: FieldType -> Analyser
pushes ft = modify $ \f -> f{stack = StackEntry ft : f.stack}

-- | Pushes a raw StackEntry
pushesEntry :: StackEntry -> Analyser
pushesEntry se = modify $ \f -> f{stack = se : f.stack}

-- | Pushes an item only if it exists (for void returns)
pushesMaybe :: Maybe FieldType -> Analyser
pushesMaybe Nothing = pure ()
pushesMaybe (Just ft) = pushes ft

-- | pops 1, pushes 1
replaceTop :: FieldType -> Analyser
replaceTop ft = do
    pops 1
    pushes ft

-- | Loads a local variable onto the stack
loads :: String -> U1 -> Analyser
loads instName i = do
    f <- get
    block <- ask
    if i >= genericLength f.locals
        then
            indexOOBError instName i f block
        else do
            let lv = f.locals !! fromIntegral i
            pushesEntry (lvToStackEntry lv)

-- | Stores the top of the stack into a local variable
stores ::
    -- | The variable index
    Int ->
    Analyser
stores i = do
    f <- get
    case f.stack of
        [] -> error "Stack underflow during store"
        (top : rest) ->
            put
                f
                    { locals = replaceAtOrGrow i (stackEntryToLV top) f.locals
                    , stack = rest
                    }

-- | Error for index out of bounds
indexOOBError :: (HasCallStack) => String -> U1 -> Frame -> BasicBlock -> a
indexOOBError instName i ba block =
    error $
        showPretty
            ( vsep
                [ pretty instName <> " at index " <> pretty i <> "is out of bounds."
                , "Locals: " <> pretty ba.locals
                , "Instructions: " <> pretty block.instructions
                ]
            )
