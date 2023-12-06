{-# LANGUAGE DataKinds #-}
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
import GHC.Stack (HasCallStack)
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), returnDescriptorType)
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type (FieldType (..), PrimitiveType (..), classInfoTypeToFieldType, fieldTypeToClassInfoType)

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

data StackEntry = StackEntry FieldType | StackEntryTop | StackEntryNull
    deriving (Show, Eq)

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

analyseBlockDiff :: Frame -> BasicBlock -> Frame
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

    analyseInstruction :: Instruction -> Frame -> Frame
    analyseInstruction (Label _) ba = error "Label should not be encountered in analyseInstruction"
    analyseInstruction (ALoad i) ba =
        if i > genericLength ba.locals
            then error $ "ALoad index out of bounds. Given: " <> show i <> " Locals: " <> show ba.locals
            else ba{stack = lvToStackEntry (ba.locals !! fromIntegral i) : ba.stack}
    analyseInstruction (ILoad i) ba =
        if i > genericLength ba.locals
            then error $ "ILoad index out of bounds. Given: " <> show i <> " Locals: " <> show ba.locals
            else ba{stack = lvToStackEntry (ba.locals !! fromIntegral i) : ba.stack}
    analyseInstruction (AStore i) ba = ba{locals = replaceAtOrGrow (fromIntegral i) (stackEntryToLV $ head ba.stack) ba.locals, stack = tail ba.stack}
    analyseInstruction (IStore i) ba = ba{locals = replaceAtOrGrow (fromIntegral i) (stackEntryToLV $ head ba.stack) ba.locals, stack = tail ba.stack}
    analyseInstruction AReturn ba = ba{stack = tail ba.stack}
    analyseInstruction Return ba = ba
    analyseInstruction (LDC (LDCInt _)) ba = ba{stack = StackEntry (PrimitiveFieldType Int) : ba.stack}
    analyseInstruction AConstNull ba = ba{stack = StackEntryNull : ba.stack}
    analyseInstruction Dup ba = ba{stack = head ba.stack : ba.stack}
    analyseInstruction (IfEq _) ba = ba{stack = tail ba.stack}
    analyseInstruction (IfNe _) ba = ba{stack = tail ba.stack}
    analyseInstruction (IfLt _) ba = ba{stack = tail ba.stack}
    analyseInstruction (IfGe _) ba = ba{stack = tail ba.stack}
    analyseInstruction (IfGt _) ba = ba{stack = tail ba.stack}
    analyseInstruction (IfLe _) ba = ba{stack = tail ba.stack}
    analyseInstruction (CheckCast _) ba = ba
    analyseInstruction (InvokeStatic _ _ md) ba = ba{stack = (StackEntry <$> maybeToList (returnDescriptorType md.returnDesc)) <> drop (length md.params) ba.stack}
    analyseInstruction (InvokeVirtual _ _ md) ba = ba{stack = (StackEntry <$> maybeToList (returnDescriptorType md.returnDesc)) <> drop (1 + length md.params) ba.stack}
    analyseInstruction (InvokeInterface _ _ md) ba = ba{stack = (StackEntry <$> maybeToList (returnDescriptorType md.returnDesc)) <> drop (length md.params) ba.stack}
    analyseInstruction (InvokeDynamic _ _ md) ba = ba{stack = (StackEntry <$> maybeToList (returnDescriptorType md.returnDesc)) <> drop (1 + length md.params) ba.stack}
    analyseInstruction (PutStatic{}) ba = ba{stack = tail ba.stack}
    analyseInstruction (GetField t _ _) ba = ba{stack = StackEntry (classInfoTypeToFieldType t) : tail ba.stack}
    analyseInstruction (GetStatic t _ _) ba = ba{stack = StackEntry (classInfoTypeToFieldType t) : ba.stack}
    analyseInstruction (Goto _) ba = ba
    analyseInstruction (LDC l) ba = ba{stack = StackEntry (ldcEntryToFieldType l) : ba.stack}

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

calculateStackMapFrames :: MethodDescriptor -> [Instruction] -> [StackMapFrame]
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
