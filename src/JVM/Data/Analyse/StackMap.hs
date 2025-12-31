{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{- | Generate a stack map table for a method.
This process MUST run last in the high level stage -
modifications to the code after this point will invalidate the stack map table and cause invalid class files to be generated.
-}
module JVM.Data.Analyse.StackMap where

import Control.Lens.Fold
import Data.Generics.Sum (AsAny (_As))
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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
                pops 1 -- pop receiver
                pops (length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeInterface _ _ md -> do
                pops 1 -- pop receiver
                pops (length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeDynamic _ _ md -> do
                pops (length md.params)
                pushesMaybe (returnDescriptorType md.returnDesc)
            InvokeSpecial _ _ md -> do
                pops 1 -- pop receiver
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

-- | Diff two frames to produce a StackMapFrame
diffFrames :: Frame -> Frame -> Label -> StackMapFrame
diffFrames (Frame locals1 stack1) (Frame locals2 stack2) label
    -- locals the same, stack empty
    | locals1 == locals2 && null stack2 = SameFrame label
    -- same locals, one stack item (previous stack must be empty)
    | [x] <- stack2, locals1 == locals2, null stack1 = SameLocals1StackItemFrame (seToVerificationTypeInfo x) label
    -- stack empty, locals appended
    | null stack2
        && locals1 `isPrefixOf` locals2
        && length locals2 - length locals1 <= 3 =
        let difference = drop (length locals1) locals2
         in AppendFrame (map lvToVerificationTypeInfo difference) label
    -- stack empty, locals chopped
    | null stack2
        && locals2 `isPrefixOf` locals1
        && length locals1 - length locals2 <= 3 =
        ChopFrame (fromIntegral $ length locals1 - length locals2) label
    -- full frame otherwise
    | otherwise = FullFrame (map lvToVerificationTypeInfo locals2) (map seToVerificationTypeInfo stack2) label

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

-- | Merge two frames that could both reach the same point.
-- Returns Nothing if frames are identical, Just merged if they differ.
mergeFrames :: Frame -> Frame -> Maybe Frame
mergeFrames (Frame locals1 stack1) (Frame locals2 stack2)
    | locals1 == locals2 && stack1 == stack2 = Nothing
    | otherwise = Just $ Frame
        { locals = zipWithDefault Uninitialised mergeLocal locals1 locals2
        , stack = if stack1 == stack2 then stack1 else []  -- Conservative: empty if different
        }
  where
    mergeLocal :: LocalVariable -> LocalVariable -> LocalVariable
    mergeLocal Uninitialised x = x
    mergeLocal x Uninitialised = x
    mergeLocal x y = if x == y then x else Uninitialised

    zipWithDefault :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
    zipWithDefault def f xs ys = go xs ys
      where
        go [] [] = []
        go (a : as) [] = f a def : go as []
        go [] (b : bs) = f def b : go [] bs
        go (a : as) (b : bs) = f a b : go as bs

-- | Get successors of a block (labels that can be reached from this block)
getSuccessors :: BasicBlock -> [Maybe Label]
getSuccessors block =
    let lastInst = if null block.instructions then Nothing else Just (last block.instructions)
        jump = lastInst >>= jumpTarget
        fallThrough = if isTerminator lastInst then Nothing else block.end
    in [jump, fallThrough]
  where
    isTerminator (Just (Goto _)) = True
    isTerminator (Just AReturn) = True
    isTerminator (Just Return) = True
    isTerminator _ = False

-- | Build a map from labels to block indices
buildLabelToBlockMap :: [BasicBlock] -> Map Label Int
buildLabelToBlockMap blocks = Map.fromList
    [(label, block.index) | block <- blocks, Just label <- [block.end]]

-- | Worklist algorithm to compute frames at each block entry
computeBlockFrames :: Frame -> [BasicBlock] -> Map Int Frame
computeBlockFrames initialFrame blocks =
    let labelToBlock = buildLabelToBlockMap blocks
        blockArray = Map.fromList [(b.index, b) | b <- blocks]
        
        -- Initialize: block 0 has initialFrame
        initialFrames = Map.singleton 0 initialFrame
        initialWorklist = Set.singleton 0
        
    in worklistLoop initialWorklist initialFrames labelToBlock blockArray
  where
    worklistLoop :: Set Int -> Map Int Frame -> Map Label Int -> Map Int BasicBlock -> Map Int Frame
    worklistLoop worklist frames labelToBlock blockArray
        | Set.null worklist = frames
        | otherwise =
            let (blockIdx, worklist') = Set.deleteFindMin worklist
                block = blockArray Map.! blockIdx
                inputFrame = frames Map.! blockIdx
                outputFrame = analyseBlockDiff inputFrame block
                
                -- Get successors
                successorLabels = getSuccessors block
                successorIndices = mapMaybe (\lbl -> lbl >>= (`Map.lookup` labelToBlock)) successorLabels
                
                -- Propagate to successors
                (frames', worklist'') = foldl' (propagateFrame outputFrame) (frames, worklist') successorIndices
                
            in worklistLoop worklist'' frames' labelToBlock blockArray
    
    propagateFrame :: Frame -> (Map Int Frame, Set Int) -> Int -> (Map Int Frame, Set Int)
    propagateFrame outFrame (frames, worklist) succIdx =
        case Map.lookup succIdx frames of
            Nothing ->
                -- First time reaching this block
                (Map.insert succIdx outFrame frames, Set.insert succIdx worklist)
            Just existingFrame ->
                case mergeFrames existingFrame outFrame of
                    Nothing -> (frames, worklist)  -- No change
                    Just mergedFrame ->
                        -- Frame changed, update and add to worklist
                        (Map.insert succIdx mergedFrame frames, Set.insert succIdx worklist)

calculateStackMapFrames :: (HasCallStack) => MethodDescriptor -> [Instruction] -> [StackMapFrame]
calculateStackMapFrames md code = do
    let blocks = splitIntoBasicBlocks code
    let top = topFrame md
    
    -- Use worklist algorithm to compute frame at entry of each block
    let blockFrames = computeBlockFrames top blocks
    
    -- Extract frames for blocks that have labels (jump targets), in order
    let labelledFrames =
            [ (blockFrames Map.! block.index, label)
            | block <- blocks
            , Just label <- [block.end]
            , Map.member block.index blockFrames
            ]
    
    -- Generate stack map frames as deltas from previous frame
    case labelledFrames of
        [] -> []
        ((firstFrame, firstLabel) : rest) ->
            let firstSMF = diffFrames top firstFrame firstLabel
                restSMFs = zipWith
                    (\(prevFrame, _) (currFrame, currLabel) -> diffFrames prevFrame currFrame currLabel)
                    labelledFrames
                    rest
            in firstSMF : restSMFs

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
