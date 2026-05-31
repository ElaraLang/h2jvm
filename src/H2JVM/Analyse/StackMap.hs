{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{- | Generate a stack map table for a method.
This process MUST run last in the high level stage,
modifications to the code after this point will invalidate the stack map table and cause invalid class files to be generated.
-}
module H2JVM.Analyse.StackMap (
    calculateStackMapFrames,
    BasicBlock (..),
    Frame (..),
    LocalVariable (..),
    analyseBlockDiff,
    diffFrames,
    splitIntoBasicBlocks,
    topFrame,
    StackMapError (..),
)
where

import Control.Monad (foldM, (>=>))
import Data.Foldable (foldlM)
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Maybe (isNothing, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (for)
import Effectful (Eff, runPureEff)
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.State.Static.Local (State, execState, get, gets, modify, put)
import Witch

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import H2JVM.Builder.Label
import H2JVM.ClassFile.AccessFlags (MethodAccessFlag (..))
import H2JVM.ClassFile.Method
import H2JVM.Descriptor (MethodDescriptor (..), returnDescriptorType)
import H2JVM.Instruction
import H2JVM.Internal.Pretty (Pretty (pretty))
import H2JVM.Internal.Raw.Types
import H2JVM.Internal.Util (bug)
import H2JVM.Name (QualifiedClassName)
import H2JVM.Type (FieldType (..), PrimitiveType (..), classInfoTypeToFieldType, fieldTypeToClassInfoType)

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
    deriving (Eq, Show)

-- | Represents the JVM frame state at a particular program point.
data Frame = Frame
    { locals :: [LocalVariable]
    -- ^ The local variable slots
    , stack :: [StackEntry]
    -- ^ The operand stack, with the top of stack at the head
    }
    deriving (Eq, Show)

-- | Represents the type of a local variable slot in the stack frame
data LocalVariable
    = -- | The slot has not been initialised or contains an unusable value
      Uninitialised
    | -- | The slot contains a value of the given type
      LocalVariable FieldType
    deriving (Eq, Show)

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
    deriving (Eq, Show)

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

-- | Split a list of instructions into basic blocks, by splitting at labels and branch instructions.
splitIntoBasicBlocks :: HasCallStack => NonEmpty Instruction -> NonEmpty BasicBlock
splitIntoBasicBlocks l =
    let blockData = splitOnLabels (NE.toList l)
        startLabels = fmap fst blockData
        instructions = fmap snd blockData
        endLabels = drop 1 startLabels ++ [Nothing]
        blocksList = zipWith4 BasicBlock [0 ..] instructions startLabels endLabels
     in case blocksList of
            (b : bs) -> b :| bs
            [] -> bug "splitIntoBasicBlocks: splitOnLabels produced 0 blocks from NonEmpty input"

-- | Build a map from labels to the index of the block that starts with that label.
buildLabelToBlockMap :: NonEmpty BasicBlock -> Map Label Int
buildLabelToBlockMap blocks =
    Map.fromList
        [(label, block.index) | block <- NE.toList blocks, Just label <- [block.start]]

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
    isBranchInstruction (If _) = True
    isBranchInstruction (IfICmp _) = True
    isBranchInstruction (Goto _) = True
    isBranchInstruction AReturn = True
    isBranchInstruction Return = True
    isBranchInstruction _ = False

{- | Compute the initial frame state at method entry (the "top frame"),
based on the method descriptor and access flags (static vs instance).
-}
topFrame :: QualifiedClassName -> [MethodAccessFlag] -> MethodDescriptor -> Frame
topFrame thisType flags (MethodDescriptor args _) =
    Frame (expandLocals $ adjustForThis flags args) []
  where
    adjustForThis :: [MethodAccessFlag] -> [FieldType] -> [FieldType]
    adjustForThis flags params =
        if MStatic `elem` flags
            then params
            else ObjectFieldType thisType : params -- instance methods have @this@ as a local
    expandLocals :: [FieldType] -> [LocalVariable]
    expandLocals [] = []
    expandLocals (PrimitiveFieldType JDouble : ts) =
        LocalVariable (PrimitiveFieldType JDouble) : Uninitialised : expandLocals ts
    expandLocals (PrimitiveFieldType JLong : ts) =
        LocalVariable (PrimitiveFieldType JLong) : Uninitialised : expandLocals ts
    expandLocals (t : ts) = LocalVariable t : expandLocals ts

-- | Remove any implicit 'top' entries that are used for padding wide types in the locals list
filterImplicitTops :: [LocalVariable] -> [LocalVariable]
filterImplicitTops [] = []
filterImplicitTops (LocalVariable (PrimitiveFieldType JDouble) : Uninitialised : rest) =
    LocalVariable (PrimitiveFieldType JDouble) : filterImplicitTops rest
filterImplicitTops (LocalVariable (PrimitiveFieldType JLong) : Uninitialised : rest) =
    LocalVariable (PrimitiveFieldType JLong) : filterImplicitTops rest
filterImplicitTops (x : xs) = x : filterImplicitTops xs

-- | Compute the frame state after executing all instructions in a basic block.
analyseBlockDiff :: HasCallStack => Frame -> BasicBlock -> Either StackMapError Frame
analyseBlockDiff current block = foldM (flip analyseInstruction) current block.instructions
  where
    analyseInstruction :: HasCallStack => Instruction -> Frame -> Either StackMapError Frame
    analyseInstruction inst frame =
        runPureEff $
            runErrorNoCallStack @StackMapError $
                runReader block $
                    execState frame $
                        analyse inst

-- | analyse a single instruction's effect on the stack and locals
analyse :: HasCallStack => Instruction -> Analyser
analyse = \case
    (Label _) -> bug "Label should not be encountered in analyseInstruction"
    inst@AALoad -> do
        s <- gets (.stack)
        case s of
            (_index : StackEntry (ArrayFieldType innerType) : _) -> do
                pops 2 -- index, arrayref
                pushes innerType
            (_index : StackEntryTop : _) ->
                throwError $ InvalidStackState inst s (Just "AAload: arrayref is Uninitialised (Top)")
            (_index : StackEntryNull : _) -> do
                -- this will npe at runtime but isn't technically invalid
                pops 2
                pushesEntry StackEntryTop
            _ -> throwError $ StackUnderflow inst
    ArrayLength -> do
        pops 1 -- arrayref
        pushes (PrimitiveFieldType JInt)
    inst@(ALoad i) -> loads inst i
    inst@(ILoad i) -> loads inst i
    inst@(AStore i) -> stores inst (into i)
    inst@(IStore i) -> stores inst (into i)
    AReturn -> pops 1
    IReturn -> pops 1
    AConstNull -> pushesEntry StackEntryNull
    Return -> pure ()
    LDC t -> pushes (ldcEntryToFieldType t)
    IConst0 -> pushes (PrimitiveFieldType JInt)
    IConst1 -> pushes (PrimitiveFieldType JInt)
    inst@Dup -> do
        s <- gets (.stack)
        case s of
            [] -> throwError $ StackUnderflow inst
            head : _ -> pushesEntry head
    IAnd -> do
        pops 2
        pushes (PrimitiveFieldType JInt)
    IOr -> do
        pops 2
        pushes (PrimitiveFieldType JInt)
    If _ -> pops 1
    CheckCast ft -> replaceTop (classInfoTypeToFieldType ft)
    Instanceof _ -> replaceTop (PrimitiveFieldType JInt)
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
    IfICmp _cmp -> pops 2
    IAdd -> do
        pops 2
        pushes (PrimitiveFieldType JInt)
    ISub -> do
        pops 2
        pushes (PrimitiveFieldType JInt)
    IMul -> do
        pops 2
        pushes (PrimitiveFieldType JInt)
    IDiv -> do
        pops 2
        pushes (PrimitiveFieldType JInt)

-- | Compute the delta between two frames to produce a StackMapFrame
diffFrames :: Frame -> Frame -> Label -> StackMapFrame
diffFrames (Frame locals1 _stack1) (Frame locals2 stack2) label
    | locals1 == locals2 && null stack2 = SameFrame label
    -- same locals, one stack item
    | [x] <- stack2, locals1 == locals2 = SameLocals1StackItemFrame (seToVerificationTypeInfo x) label
    -- stack empty, locals appended
    | null stack2
        && locals1 `isPrefixOf` locals2
        && let diff = length (filterImplicitTops locals2) - length (filterImplicitTops locals1) in diff > 0 && diff <= 3 =
        let difference = drop (length locals1) locals2
         in AppendFrame (map lvToVerificationTypeInfo (filterImplicitTops difference)) label
    | null stack2
        && locals2 `isPrefixOf` locals1
        && let diff = length (filterImplicitTops locals1) - length (filterImplicitTops locals2)
            in diff > 0 && diff <= 3 =
        ChopFrame (unsafeInto $ length locals1 - length locals2) label
    | otherwise =
        FullFrame
            (map lvToVerificationTypeInfo (filterImplicitTops locals2))
            (map seToVerificationTypeInfo stack2)
            label

-- | Convert a local variable to its JVM verification type info representation.
lvToVerificationTypeInfo :: LocalVariable -> VerificationTypeInfo
lvToVerificationTypeInfo Uninitialised = TopVariableInfo
lvToVerificationTypeInfo (LocalVariable ft) = case ft of
    PrimitiveFieldType JInt -> IntegerVariableInfo
    PrimitiveFieldType JByte -> IntegerVariableInfo
    PrimitiveFieldType JChar -> IntegerVariableInfo
    PrimitiveFieldType JShort -> IntegerVariableInfo
    PrimitiveFieldType JBoolean -> IntegerVariableInfo
    PrimitiveFieldType JFloat -> FloatVariableInfo
    PrimitiveFieldType JLong -> LongVariableInfo
    PrimitiveFieldType JDouble -> DoubleVariableInfo
    ObjectFieldType{} -> ObjectVariableInfo (fieldTypeToClassInfoType ft)
    ArrayFieldType{} -> ObjectVariableInfo (fieldTypeToClassInfoType ft)

-- | Convert a stack entry to its JVM verification type info representation.
seToVerificationTypeInfo :: StackEntry -> VerificationTypeInfo
seToVerificationTypeInfo StackEntryTop = TopVariableInfo
seToVerificationTypeInfo StackEntryNull = NullVariableInfo
seToVerificationTypeInfo (StackEntry ft) = case ft of
    PrimitiveFieldType JInt -> IntegerVariableInfo
    PrimitiveFieldType JByte -> IntegerVariableInfo
    PrimitiveFieldType JChar -> IntegerVariableInfo
    PrimitiveFieldType JShort -> IntegerVariableInfo
    PrimitiveFieldType JBoolean -> IntegerVariableInfo
    PrimitiveFieldType JFloat -> FloatVariableInfo
    PrimitiveFieldType JLong -> LongVariableInfo
    PrimitiveFieldType JDouble -> DoubleVariableInfo
    _ -> ObjectVariableInfo (fieldTypeToClassInfoType ft)

{- | Merge two frames that could both reach the same program point.
Returns 'Nothing' if frames are identical, 'Just' the merged frame otherwise.
Uses a conservative merge where differing types become 'Uninitialised'.
-}
mergeFrames :: HasCallStack => Frame -> Frame -> Either StackMapError (Maybe Frame)
mergeFrames frame1@(Frame locals1 stack1) frame2@(Frame locals2 stack2)
    | locals1 == locals2 && stack1 == stack2 = pure Nothing
    | length stack1 /= length stack2 = Left $ IncompatibleFrameMerge frame1 frame2
    | otherwise =
        pure $
            Just $
                Frame
                    { locals = zipWithDefault Uninitialised mergeLocal locals1 locals2
                    , stack = zipWith mergeStack stack1 stack2
                    }
  where
    mergeLocal :: LocalVariable -> LocalVariable -> LocalVariable
    mergeLocal Uninitialised _ = Uninitialised
    mergeLocal _ Uninitialised = Uninitialised
    mergeLocal x y = if x == y then x else Uninitialised

    mergeStack :: StackEntry -> StackEntry -> StackEntry
    mergeStack x y = if x == y then x else StackEntryTop

    zipWithDefault :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
    zipWithDefault def f = go
      where
        go [] [] = []
        go (a : as) [] = f a def : go as []
        go [] (b : bs) = f def b : go [] bs
        go (a : as) (b : bs) = f a b : go as bs

{- | Get the indices of successor blocks (blocks reachable from this block).
Includes both jump targets and fallthrough to the next block, if the last instruction is not a terminator.
-}
getSuccessors :: Map Label Int -> Int -> BasicBlock -> [Int]
getSuccessors labelToBlock blockIdx block =
    let jumpTargetIndices = mapMaybe (jumpTarget >=> (`Map.lookup` labelToBlock)) block.instructions
        lastInst = if null block.instructions then Nothing else Just (last block.instructions)
        fallThroughIdx = if isTerminator lastInst then Nothing else Just (blockIdx + 1)
     in jumpTargetIndices <> maybeToList fallThroughIdx
  where
    isTerminator (Just (Goto _)) = True
    isTerminator (Just AReturn) = True
    isTerminator (Just Return) = True
    isTerminator _ = False

{- | Compute the frame state at the entry of each basic block using a worklist algorithm.
Handles control flow merges by conservatively merging frame states with 'mergeFrames'.
-}
computeBlockFrames :: HasCallStack => Frame -> NonEmpty BasicBlock -> Either StackMapError (Map Int Frame)
computeBlockFrames initialFrame blocks = do
    let labelToBlock = buildLabelToBlockMap blocks
        blockArray = Map.fromList [(b.index, b) | b <- NE.toList blocks]
        numBlocks = length (NE.toList blocks)
        initialFrames = Map.singleton 0 initialFrame
        initialWorklist = Set.singleton 0
     in worklistLoop initialWorklist initialFrames labelToBlock blockArray numBlocks
  where
    worklistLoop :: HasCallStack => Set Int -> Map Int Frame -> Map Label Int -> Map Int BasicBlock -> Int -> Either StackMapError (Map Int Frame)
    worklistLoop worklist frames labelToBlock blockArray numBlocks
        | Set.null worklist = pure frames
        | otherwise = do
            let (blockIdx, worklist') = Set.deleteFindMin worklist
                block = blockArray Map.! blockIdx
                inputFrame = frames Map.! blockIdx
            outputFrame <- analyseBlockDiff inputFrame block
            let successorIndices = filter (< numBlocks) $ getSuccessors labelToBlock blockIdx block
            (frames', worklist'') <- foldlM (propagateFrame outputFrame) (frames, worklist') successorIndices
            worklistLoop worklist'' frames' labelToBlock blockArray numBlocks

    propagateFrame :: Frame -> (Map Int Frame, Set Int) -> Int -> Either StackMapError (Map Int Frame, Set Int)
    propagateFrame outFrame (frames, worklist) succIdx =
        case Map.lookup succIdx frames of
            Nothing ->
                Right (Map.insert succIdx outFrame frames, Set.insert succIdx worklist)
            Just existingFrame -> do
                merged <- mergeFrames existingFrame outFrame
                pure $ case merged of
                    Nothing -> (frames, worklist)
                    Just mergedFrame ->
                        (Map.insert succIdx mergedFrame frames, Set.insert succIdx worklist)

{- | Calculate the stack map frames for a method's bytecode.
This function works by first splitting the code into basic blocks,
then computing the frame state at the start of each block using a dataflow analysis,
and finally generating the stack map frames by comparing the frame states at block entries.
-}
calculateStackMapFrames ::
    HasCallStack =>
    -- | The class containing this method
    QualifiedClassName ->
    -- | Method access flags (to determine if static)
    [MethodAccessFlag] ->
    -- | The method descriptor
    MethodDescriptor ->
    -- | The method's instruction list
    NonEmpty Instruction ->
    -- | (stack map frames, max stack, max locals)
    Either StackMapError ([StackMapFrame], Int, Int)
calculateStackMapFrames enclosingClassName flags md code = do
    let blocks = splitIntoBasicBlocks code
    let top = topFrame enclosingClassName flags md

    blockFrames <- computeBlockFrames top blocks

    (maxStack, maxLocals) <- calculateMethodMaxes blockFrames blocks

    let labelToBlockIdx = buildLabelToBlockMap blocks
    let orderedPairs =
            [ (label, blockFrames Map.! blockIdx)
            | block <- NE.toList blocks
            , Just label <- [block.start]
            , Just blockIdx <- [Map.lookup label labelToBlockIdx]
            , Map.member blockIdx blockFrames
            , blockIdx > 0
            ]

    case orderedPairs of
        [] -> pure ([], maxStack, maxLocals)
        ((firstLabel, firstFrame) : rest) ->
            let firstSMF = diffFrames top firstFrame firstLabel
                restSMFs =
                    zipWith
                        (\(_, prevFrame) (currLabel, currFrame) -> diffFrames prevFrame currFrame currLabel)
                        orderedPairs
                        rest
             in pure (firstSMF : restSMFs, maxStack, maxLocals)

-- | Replace element at index in list, growing with 'Uninitialised' if needed.
replaceAtOrGrow :: Int -> LocalVariable -> [LocalVariable] -> [LocalVariable]
replaceAtOrGrow i x xs
    | i < length xs = replaceAt i x xs
    | otherwise = xs <> replicate (i - length xs) Uninitialised <> [x]

{- | Replace element at index i in list. Appends to end if i is out of bounds.
>>> replaceAt 1 'x' "abc"
"axc"
>>> replaceAt 5 'x' "abc"
"abcx"
-}
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs <> [x] <> drop (i + 1) xs

{- | Effect stack for analysing instruction effects on the frame state.
We can modify the current frame and read the current basic block.
This monad acts a mini embedded DSL for describing the effect of each instruction on the frame.
-}
type Analyser = Eff '[State Frame, Reader BasicBlock, Error StackMapError] ()

data StackMapError
    = StackUnderflow Instruction
    | InvalidStackState Instruction [StackEntry] (Maybe Text)
    | LocalIndexOutOfBounds Instruction U2 Frame BasicBlock
    | IncompatibleFrameMerge Frame Frame
    | MissingBlockFrame Int
    deriving (Show)

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
loads :: Instruction -> U2 -> Analyser
loads inst i = do
    frame <- get
    block <- ask
    if i >= genericLength frame.locals
        then
            throwError $ LocalIndexOutOfBounds inst i frame block
        else do
            let lv = frame.locals !! into i
            pushesEntry (lvToStackEntry lv)

-- | Stores the top of the stack into a local variable at the given index
stores :: Instruction -> Int -> Analyser
stores inst i = do
    f <- get
    case f.stack of
        [] -> throwError $ StackUnderflow inst
        (top : rest) -> do
            let lv = stackEntryToLV top
            let locals' = replaceAtOrGrow i lv f.locals
            let finalLocals = case lv of
                    LocalVariable (PrimitiveFieldType JDouble) -> replaceAtOrGrow (i + 1) Uninitialised locals'
                    LocalVariable (PrimitiveFieldType JLong) -> replaceAtOrGrow (i + 1) Uninitialised locals'
                    _ -> locals'
            put
                f
                    { locals = finalLocals
                    , stack = rest
                    }

-- | How many stack slots does a given 'FieldType' take up?
fieldTypeSlotSize :: FieldType -> Int
fieldTypeSlotSize (PrimitiveFieldType JDouble) = 2
fieldTypeSlotSize (PrimitiveFieldType JLong) = 2
fieldTypeSlotSize _ = 1

-- | How many stack slots does a given 'StackEntry' take up?
seSlotSize :: StackEntry -> Int
seSlotSize (StackEntry ft) = fieldTypeSlotSize ft
seSlotSize _ = 1

-- | How many stack slots does a given 'LocalVariable' take up?
lvSlotSize :: LocalVariable -> Int
lvSlotSize (LocalVariable ft) = fieldTypeSlotSize ft
lvSlotSize _ = 1

-- | Physical size of the operand stack of a 'Frame'
frameStackSize :: Frame -> Int
frameStackSize f = sum (map seSlotSize f.stack)

-- | Physical size of the local variables array of a 'Frame'
frameLocalsSize :: Frame -> Int
frameLocalsSize f =
    case reverse f.locals of
        [] -> 0
        (lastLv : _) -> length f.locals - 1 + lvSlotSize lastLv

-- | Calculate the max stack and max locals across all intermediate instruction frames.
calculateMethodMaxes ::
    -- | Map from block index to frame at block entry
    Map Int Frame ->
    -- | The method's basic blocks
    NonEmpty BasicBlock ->
    -- | (max stack, max locals)
    Either StackMapError (Int, Int)
calculateMethodMaxes blockFrames blocks = do
    allFrames <- fmap concat $ for (NE.toList blocks) $ \block -> do
        startFrame <-
            maybe
                (Left $ MissingBlockFrame block.index)
                Right
                (Map.lookup block.index blockFrames)
        scanlM
            (\f inst -> runPureEff $ runErrorNoCallStack $ runReader block $ execState f $ analyse inst)
            startFrame
            block.instructions
    pure
        ( maximum (0 : map frameStackSize allFrames)
        , maximum (0 : map frameLocalsSize allFrames)
        )

scanlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanlM _ z [] = pure [z]
scanlM f z (x : xs) = do
    z' <- f z x
    (z :) <$> scanlM f z' xs
