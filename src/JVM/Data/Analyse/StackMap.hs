{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{- | Generate a stack map table for a method.
This process MUST run last in the high level stage -
modifications to the code after this point will invalidate the stack map table and cause invalid class files to be generated.
-}
module JVM.Data.Analyse.StackMap where

import Control.Lens.Fold
import Control.Monad ((>=>))
import Data.Generics.Sum (AsAny (_As))
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful (Eff, runPureEff)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.State.Static.Local (State, execState, get, gets, modify, put)
import GHC.Stack (HasCallStack)
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ClassFile.AccessFlags (MethodAccessFlag (..))
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), returnDescriptorType)
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Name (QualifiedClassName)
import JVM.Data.Abstract.Type (FieldType (..), PrimitiveType (..), classInfoTypeToFieldType, fieldTypeToClassInfoType)
import JVM.Data.Pretty (Pretty (pretty), showPretty)
import JVM.Data.Raw.Types
import Prettyprinter (vsep)

{- | A basic block is a sequence of instructions with a single entry and exit point.
Control flow only enters at the beginning and exits at the end.
-}
data BasicBlock = BasicBlock
    { index :: Int
    -- ^ The index of this block in the method's block list
    , instructions :: [Instruction]
    -- ^ The instructions in this block (excluding the starting label)
    , start :: Maybe Label
    -- ^ The label at the start of this block, if any
    , end :: Maybe Label
    -- ^ The label that the next block starts with, if any
    }
    deriving (Show, Eq)

{- | Represents the JVM frame state at a particular program point.
Used for computing stack map frames required by the JVM verifier.
-}
data Frame = Frame
    { locals :: [LocalVariable]
    -- ^ The local variable slots, indexed from 0
    , stack :: [StackEntry]
    -- ^ The operand stack, with the top of stack at the head
    }
    deriving (Show, Eq)

-- | Represents the type of a local variable slot in the stack frame
data LocalVariable
    = -- | The slot has not been initialised or contains an unusable value
      Uninitialised
    | -- | The slot contains a value of the given type
      LocalVariable FieldType
    deriving (Show, Eq)

instance Pretty LocalVariable where
    pretty Uninitialised = "uninitialised"
    pretty (LocalVariable ft) = pretty ft

-- | Represents the type of a stack slot.
data StackEntry
    = -- | A typed value on the stack
      StackEntry FieldType
    | -- | An unusable or uninitialised stack slot
      StackEntryTop
    | -- | A null reference on the stack
      StackEntryNull
    deriving (Show, Eq)

instance Pretty StackEntry where
    pretty StackEntryTop = "top"
    pretty StackEntryNull = "null"
    pretty (StackEntry ft) = pretty ft

-- | Convert a local variable to a stack entry
lvToStackEntry :: LocalVariable -> StackEntry
lvToStackEntry Uninitialised = StackEntryTop
lvToStackEntry (LocalVariable ft) = StackEntry ft

-- | Convert a stack entry to a local variable
stackEntryToLV :: StackEntry -> LocalVariable
stackEntryToLV StackEntryTop = Uninitialised
stackEntryToLV StackEntryNull = Uninitialised
stackEntryToLV (StackEntry ft) = LocalVariable ft

{- | Split a list of instructions into basic blocks.
Blocks are split at labels and after branch/return instructions.
-}
splitIntoBasicBlocks :: [Instruction] -> [BasicBlock]
splitIntoBasicBlocks [] = []
splitIntoBasicBlocks l =
    let blockData = splitOnLabels l
        startLabels = map fst blockData
        instructions = map snd blockData
        endLabels = tail startLabels ++ [Nothing]
     in zipWith4 BasicBlock [0 ..] instructions startLabels endLabels

-- | Build a map from labels to the index of the block that starts with that label.
buildLabelToBlockMap :: [BasicBlock] -> Map Label Int
buildLabelToBlockMap blocks =
    Map.fromList
        [(label, block.index) | block <- blocks, Just label <- [block.start]]

{- | Split instructions into groups, each starting with an optional label.
Also splits after branch instructions to ensure proper block boundaries.
-}
splitOnLabels :: [Instruction] -> [(Maybe Label, [Instruction])]
splitOnLabels xs = go xs [] Nothing
  where
    go :: [Instruction] -> [Instruction] -> Maybe Label -> [(Maybe Label, [Instruction])]
    go [] acc label
        | null acc && isNothing label = []
        | otherwise = [(label, reverse acc)]
    go (x : xs) acc label = case x of
        Label l' ->
            if null acc && isNothing label
                then go xs [] (Just l')
                else (label, reverse acc) : go xs [] (Just l')
        _other ->
            let acc' = x : acc
             in if isBranchInstruction x
                    then (label, reverse acc') : go xs [] Nothing
                    else go xs acc' label

    isBranchInstruction :: Instruction -> Bool
    isBranchInstruction (IfEq _) = True
    isBranchInstruction (IfNe _) = True
    isBranchInstruction (IfLt _) = True
    isBranchInstruction (IfGe _) = True
    isBranchInstruction (IfGt _) = True
    isBranchInstruction (IfLe _) = True
    isBranchInstruction (Goto _) = True
    isBranchInstruction AReturn = True
    isBranchInstruction Return = True
    isBranchInstruction _ = False

{- | Compute the initial frame state at method entry.
Includes @this@ reference for instance methods, followed by method parameters.
-}
topFrame :: QualifiedClassName -> [MethodAccessFlag] -> MethodDescriptor -> Frame
topFrame thisType flags (MethodDescriptor args _) =
    Frame (map LocalVariable $ adjustForThis flags args) []
  where
    adjustForThis :: [MethodAccessFlag] -> [FieldType] -> [FieldType]
    adjustForThis flags params =
        if MStatic `elem` flags
            then params
            else ObjectFieldType thisType : params

{- | Compute the frame state after executing all instructions in a basic block.
Takes the frame state at block entry and returns the state at block exit.
-}
analyseBlockDiff :: (HasCallStack) => Frame -> BasicBlock -> Frame
analyseBlockDiff current block = foldl' (flip analyseInstruction) current block.instructions
  where
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
            IReturn -> pops 1
            AConstNull -> pushesEntry StackEntryNull
            Return -> pure ()
            LDC t -> pushes (ldcEntryToFieldType t)
            IConst0 -> pushes (PrimitiveFieldType Int)
            IConst1 -> pushes (PrimitiveFieldType Int)
            Dup -> do
                s <- gets (.stack)
                case s of
                    [] -> error "Stack underflow during dup"
                    head : _ -> pushesEntry head
            IAnd -> do
                pops 2
                pushes (PrimitiveFieldType Int)
            IOr -> do
                pops 2
                pushes (PrimitiveFieldType Int)
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
                pops 1
                pops (length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeInterface _ _ md -> do
                pops 1
                pops (length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeDynamic _ _ md -> do
                pops (length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeSpecial _ _ md -> do
                pops 1
                pops (length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            PutStatic{} -> pops 1
            GetField _ _ ft -> do
                pops 1
                pushes ft
            GetStatic _ _ ft -> pushes ft
            PutField{} -> pops 2
            Goto _ -> pure ()
            New t -> pushes (classInfoTypeToFieldType t)

-- | Compute the delta between two frames to produce a StackMapFrame
diffFrames :: Frame -> Frame -> Label -> StackMapFrame
diffFrames (Frame locals1 stack1) (Frame locals2 stack2) label
    | locals1 == locals2 && null stack2 = SameFrame label
    -- same locals, one stack item (previous stack must be empty)
    | [x] <- stack2, locals1 == locals2, null stack1 = SameLocals1StackItemFrame (seToVerificationTypeInfo x) label
    -- stack empty, locals appended
    | null stack2
        && locals1 `isPrefixOf` locals2
        && let diff = length locals2 - length locals1 in diff > 0 && diff <= 3 =
        let difference = drop (length locals1) locals2
         in AppendFrame (map lvToVerificationTypeInfo difference) label
    | null stack2
        && locals2 `isPrefixOf` locals1
        && length locals1 - length locals2 <= 3 =
        ChopFrame (fromIntegral $ length locals1 - length locals2) label
    | otherwise = FullFrame (map lvToVerificationTypeInfo locals2) (map seToVerificationTypeInfo stack2) label

-- | Convert a local variable to its JVM verification type info representation.
lvToVerificationTypeInfo :: LocalVariable -> VerificationTypeInfo
lvToVerificationTypeInfo Uninitialised = TopVariableInfo
lvToVerificationTypeInfo (LocalVariable ft) = case ft of
    PrimitiveFieldType Int -> IntegerVariableInfo
    PrimitiveFieldType Byte -> IntegerVariableInfo
    PrimitiveFieldType Char -> IntegerVariableInfo
    PrimitiveFieldType Short -> IntegerVariableInfo
    PrimitiveFieldType Boolean -> IntegerVariableInfo
    PrimitiveFieldType Float -> FloatVariableInfo
    PrimitiveFieldType Long -> LongVariableInfo
    PrimitiveFieldType Double -> DoubleVariableInfo
    ObjectFieldType{} -> ObjectVariableInfo (fieldTypeToClassInfoType ft)
    ArrayFieldType{} -> ObjectVariableInfo (fieldTypeToClassInfoType ft)

-- | Convert a stack entry to its JVM verification type info representation.
seToVerificationTypeInfo :: StackEntry -> VerificationTypeInfo
seToVerificationTypeInfo StackEntryTop = TopVariableInfo
seToVerificationTypeInfo StackEntryNull = NullVariableInfo
seToVerificationTypeInfo (StackEntry ft) = case ft of
    PrimitiveFieldType Int -> IntegerVariableInfo
    PrimitiveFieldType Byte -> IntegerVariableInfo
    PrimitiveFieldType Char -> IntegerVariableInfo
    PrimitiveFieldType Short -> IntegerVariableInfo
    PrimitiveFieldType Boolean -> IntegerVariableInfo
    PrimitiveFieldType Float -> FloatVariableInfo
    PrimitiveFieldType Long -> LongVariableInfo
    PrimitiveFieldType Double -> DoubleVariableInfo
    _ -> ObjectVariableInfo (fieldTypeToClassInfoType ft)

{- | Merge two frames that could both reach the same program point.
Returns 'Nothing' if frames are identical, 'Just' the merged frame otherwise.
Uses a conservative merge: differing types become 'Uninitialised'.
-}
mergeFrames :: Frame -> Frame -> Maybe Frame
mergeFrames (Frame locals1 stack1) (Frame locals2 stack2)
    | locals1 == locals2 && stack1 == stack2 = Nothing
    | otherwise =
        Just $
            Frame
                { locals = zipWithDefault Uninitialised mergeLocal locals1 locals2
                , stack = if stack1 == stack2 then stack1 else []
                }
  where
    mergeLocal :: LocalVariable -> LocalVariable -> LocalVariable
    mergeLocal Uninitialised _ = Uninitialised
    mergeLocal _ Uninitialised = Uninitialised
    mergeLocal x y = if x == y then x else Uninitialised

    zipWithDefault :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
    zipWithDefault def f = go
      where
        go [] [] = []
        go (a : as) [] = f a def : go as []
        go [] (b : bs) = f def b : go [] bs
        go (a : as) (b : bs) = f a b : go as bs

{- | Get the indices of successor blocks (blocks reachable from this block).
Includes both jump targets and fall-through to the next block (if not terminated).
-}
getSuccessors :: Map Label Int -> Int -> BasicBlock -> [Int]
getSuccessors labelToBlock blockIdx block =
    let jumpTargetIndices = mapMaybe (jumpTarget >=> (`Map.lookup` labelToBlock)) block.instructions
        lastInst = if null block.instructions then Nothing else Just (last block.instructions)
        fallThroughIdx = if isTerminator lastInst then Nothing else Just (blockIdx + 1)
     in jumpTargetIndices ++ Data.Maybe.maybeToList fallThroughIdx
  where
    isTerminator (Just (Goto _)) = True
    isTerminator (Just AReturn) = True
    isTerminator (Just Return) = True
    isTerminator _ = False

{- | Compute the frame state at the entry of each basic block using a worklist algorithm.
Handles control flow merges by conservatively merging frame states.
-}
computeBlockFrames :: (HasCallStack) => Frame -> [BasicBlock] -> Map Int Frame
computeBlockFrames initialFrame blocks =
    let labelToBlock = buildLabelToBlockMap blocks
        blockArray = Map.fromList [(b.index, b) | b <- blocks]
        numBlocks = length blocks
        initialFrames = Map.singleton 0 initialFrame
        initialWorklist = Set.singleton 0
     in worklistLoop initialWorklist initialFrames labelToBlock blockArray numBlocks
  where
    worklistLoop :: (HasCallStack) => Set Int -> Map Int Frame -> Map Label Int -> Map Int BasicBlock -> Int -> Map Int Frame
    worklistLoop worklist frames labelToBlock blockArray numBlocks
        | Set.null worklist = frames
        | otherwise =
            let (blockIdx, worklist') = Set.deleteFindMin worklist
                block = blockArray Map.! blockIdx
                inputFrame = frames Map.! blockIdx
                outputFrame = analyseBlockDiff inputFrame block
                successorIndices = filter (< numBlocks) $ getSuccessors labelToBlock blockIdx block
                (frames', worklist'') = foldl' (propagateFrame outputFrame) (frames, worklist') successorIndices
             in worklistLoop worklist'' frames' labelToBlock blockArray numBlocks

    propagateFrame :: Frame -> (Map Int Frame, Set Int) -> Int -> (Map Int Frame, Set Int)
    propagateFrame outFrame (frames, worklist) succIdx =
        case Map.lookup succIdx frames of
            Nothing ->
                (Map.insert succIdx outFrame frames, Set.insert succIdx worklist)
            Just existingFrame ->
                case mergeFrames existingFrame outFrame of
                    Nothing -> (frames, worklist)
                    Just mergedFrame ->
                        (Map.insert succIdx mergedFrame frames, Set.insert succIdx worklist)

{- | Calculate the stack map frames for a method's bytecode.
This function works by first splitting the code into basic blocks,
then computing the frame state at the start of each block using a dataflow analysis,
and finally generating the stack map frames by comparing the frame states at block entries.
-}
calculateStackMapFrames ::
    (HasCallStack) =>
    -- | The class containing this method
    QualifiedClassName ->
    -- | Method access flags (to determine if static)
    [MethodAccessFlag] ->
    -- | The method descriptor
    MethodDescriptor ->
    -- | The method's instruction list
    [Instruction] ->
    [StackMapFrame]
calculateStackMapFrames enclosingClassName flags md code = do
    let blocks = splitIntoBasicBlocks code
    let top = topFrame enclosingClassName flags md
    let blockFrames = computeBlockFrames top blocks
    let labelToBlockIdx = buildLabelToBlockMap blocks
    let orderedPairs =
            [ (label, blockFrames Map.! blockIdx)
            | block <- blocks
            , Just label <- [block.start]
            , Just blockIdx <- [Map.lookup label labelToBlockIdx]
            , Map.member blockIdx blockFrames
            , blockIdx > 0
            ]

    case orderedPairs of
        [] -> []
        ((firstLabel, firstFrame) : rest) ->
            let firstSMF = diffFrames top firstFrame firstLabel
                restSMFs =
                    zipWith
                        (\(_, prevFrame) (currLabel, currFrame) -> diffFrames prevFrame currFrame currLabel)
                        orderedPairs
                        rest
             in firstSMF : restSMFs

-- | Replace element at index in list, growing with 'Uninitialised' if needed.
replaceAtOrGrow :: Int -> LocalVariable -> [LocalVariable] -> [LocalVariable]
replaceAtOrGrow i x xs
    | i < length xs = replaceAt i x xs
    | otherwise = xs <> replicate (i - length xs) Uninitialised <> [x]

-- | Replace element at index i in list. Appends to end if i is out of bounds.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs <> [x] <> drop (i + 1) xs

{- | Effect stack for analysing instruction effects on the frame state.
We can modify the current frame and read the current basic block.
-}
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

-- | Replaces the top stack entry with the given type
replaceTop :: FieldType -> Analyser
replaceTop ft = do
    pops 1
    pushes ft

-- | Loads a local variable onto the stack
loads :: String -> U2 -> Analyser
loads instName i = do
    f <- get
    block <- ask
    if i >= genericLength f.locals
        then
            indexOOBError instName i f block
        else do
            let lv = f.locals !! fromIntegral i
            pushesEntry (lvToStackEntry lv)

-- | Stores the top of the stack into a local variable at the given index
stores :: Int -> Analyser
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

-- | Report an error when a local variable index is out of bounds.
indexOOBError :: (HasCallStack) => String -> U2 -> Frame -> BasicBlock -> a
indexOOBError instName i ba block =
    error $
        showPretty
            ( vsep
                [ pretty instName <> " at index " <> pretty i <> " is out of bounds."
                , "Locals: " <> pretty ba.locals
                , "Instructions: " <> pretty block.instructions
                ]
            )
