{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LexicalNegation #-}

{- | Converts abstract instructions into raw instructions, including resolving labels into offsets.
TODO: this is very inefficient, requiring three passes over the instructions
-}
module H2JVM.Internal.Convert.Instruction (CodeConverterEff, fullyRunCodeConverter, convertInstructions, fullyResolveAbs, ConvertState (..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe
import Data.Word (Word16)
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local hiding (modifyM)
import Witch

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map

import H2JVM.Builder.Label
import H2JVM.ConstantPool (ConstantPoolEntry (..), FieldRef (..), MethodRef (..))
import H2JVM.Descriptor
import H2JVM.Instruction as Abs (IfCond (..), Instruction, Instruction' (..), LDCEntry (..))
import H2JVM.Internal.Convert.ConstantPool
import H2JVM.Internal.Convert.Monad
import H2JVM.Internal.Raw.Instruction as Raw (Instruction (..))
import H2JVM.Internal.Raw.Types (U1)
import H2JVM.Internal.Util (bug)
import H2JVM.Type

import H2JVM.Internal.Raw.MagicNumbers qualified as MagicNumbers

type CodeConverterEff r = (ConstantPool :> r, State ConvertState :> r, Error CodeConverterError :> r)

fullyRunCodeConverter :: ConvertEff r' => Eff (State ConvertState : r') a -> Eff r' a
fullyRunCodeConverter r = do
    (a, _) <- runState (ConvertState 0 Map.empty) r
    pure a

data ConvertState = ConvertState
    { currentOffset :: Word16
    , labelOffsets :: Map Label Word16
    }

countArguments :: MethodDescriptor -> Int
countArguments (MethodDescriptor args _) = 1 + sum (map countArgument args)
  where
    countArgument :: FieldType -> Int
    countArgument (PrimitiveFieldType JDouble) = 2
    countArgument (PrimitiveFieldType JLong) = 2
    countArgument _ = 1

-- | The size of an instruction in bytes, used for calculating jump offsets
instructionSize :: Abs.Instruction -> Word16
instructionSize Abs.AALoad = 1
instructionSize (Abs.ALoad n)
    | n <= 3 = 1
    | n <= 255 = 2
    | otherwise = 4 -- wide (1) + opcode (1) + index (2)
instructionSize (Abs.AStore n)
    | n <= 3 = 1
    | n <= 255 = 2
    | otherwise = 4
instructionSize (Abs.ILoad n)
    | n <= 3 = 1
    | n <= 255 = 2
    | otherwise = 4
instructionSize (Abs.IStore n)
    | n <= 3 = 1
    | n <= 255 = 2
    | otherwise = 4
instructionSize Abs.AReturn = 1
instructionSize Abs.ArrayLength = 1
instructionSize Abs.AConstNull = 1
instructionSize Abs.IAnd = 1
instructionSize (Abs.If _) = 3
instructionSize (Abs.Instanceof{}) = 3
instructionSize (Abs.InvokeStatic{}) = 3
instructionSize (Abs.InvokeVirtual{}) = 3
instructionSize (Abs.InvokeInterface{}) = 5
instructionSize (Abs.InvokeDynamic{}) = 5
instructionSize (Abs.InvokeSpecial{}) = 3
instructionSize Abs.IOr = 1
instructionSize (Abs.Label _) = 0
instructionSize (Abs.LDC _) = 3
instructionSize (Abs.PutStatic{}) = 3
instructionSize (Abs.GetField{}) = 3
instructionSize (Abs.GetStatic{}) = 3
instructionSize (Abs.PutField{}) = 3
instructionSize (Abs.CheckCast _) = 3
instructionSize Abs.Return = 1
instructionSize Abs.IReturn = 1
instructionSize Abs.IConst0 = 1
instructionSize Abs.IConst1 = 1
instructionSize Abs.Dup = 1
instructionSize (Abs.Goto _) = 3
instructionSize (Abs.New _) = 3
instructionSize (Abs.IfICmp _) = 3
instructionSize Abs.IAdd = 1
instructionSize Abs.ISub = 1
instructionSize Abs.IMul = 1
instructionSize Abs.IDiv = 1

convertInstructions :: (CodeConverterEff r, HasCallStack) => NonEmpty Abs.Instruction -> Eff r (NonEmpty Raw.Instruction)
convertInstructions xs = do
    withOffsets <- insertAllLabels xs
    insts <- traverse (resolveLabel . fmap (fmap UnresolvedLabel)) withOffsets
    finalInsts <- catMaybes <$> traverse convertInstruction (NE.toList insts)
    case NE.nonEmpty finalInsts of
        Just ne -> pure ne
        Nothing -> error "All instructions were invalid"

data MaybeResolvedLabel = ResolvedLabel Word16 | UnresolvedLabel Label

-- | An instruction with a resolved offset.
data OffsetInstruction a
    = -- | an instruction with a real offset
      OffsetInstruction
        -- | the offset.
        Word16
        -- | the instruction.
        a
    | -- | a label pseudo-instruction
      LabelInstruction a
    deriving (Eq, Functor, Ord, Show)

-- | Inserts the corresponding label offsets into the state.
insertAllLabels :: HasCallStack => CodeConverterEff r => NonEmpty Abs.Instruction -> Eff r (NonEmpty (OffsetInstruction Abs.Instruction))
insertAllLabels = traverse (\x -> incOffset x *> insertLabel x)
  where
    incOffset :: HasCallStack => CodeConverterEff r => Abs.Instruction -> Eff r ()
    incOffset (Label _) = pure () -- Label instructions have no representation in the bytecode, so they don't affect the offset
    incOffset inst = do
        offset <- gets @ConvertState (.currentOffset)
        let size = instructionSize inst
        modify (\s -> s{currentOffset = offset + size})

    insertLabel :: HasCallStack => CodeConverterEff r => Abs.Instruction -> Eff r (OffsetInstruction Abs.Instruction)
    insertLabel (Label l) = do
        currentOffset <- gets @ConvertState (.currentOffset)

        modifyM $ \s -> do
            case Map.lookup l s.labelOffsets of
                Just _ -> throwError (DuplicateLabel l currentOffset)
                Nothing -> do
                    pure (s{labelOffsets = Map.insert l currentOffset s.labelOffsets})
        pure (LabelInstruction (Label l))
    insertLabel x = do
        offset <- gets @ConvertState (.currentOffset)
        pure (OffsetInstruction (offset - instructionSize x) x)

modifyM :: State s :> r => (s -> Eff r s) -> Eff r ()
modifyM f = get >>= f >>= put

-- | Turns labels into offsets where possible
resolveLabel :: CodeConverterEff r => OffsetInstruction (Abs.Instruction' MaybeResolvedLabel) -> Eff r (OffsetInstruction (Abs.Instruction' MaybeResolvedLabel))
resolveLabel (OffsetInstruction instOffset inst) =
    OffsetInstruction instOffset <$> case inst of
        Abs.If cond -> Abs.If <$> traverse resolveLabelAbs cond
        Abs.Goto l -> Abs.Goto <$> resolveLabelAbs l
        Abs.IfICmp cond -> Abs.IfICmp <$> traverse resolveLabelAbs cond
        _ -> pure inst
resolveLabel inst@(LabelInstruction{}) = pure inst

fullyResolveAbs :: CodeConverterEff r => Label -> Eff r Word16
fullyResolveAbs l = do
    x <- resolveLabelAbs (UnresolvedLabel l)
    mustBeResolvedAbs x

-- | Attempt to resolve a label to an __absolute__ offset
resolveLabelAbs :: CodeConverterEff r => MaybeResolvedLabel -> Eff r MaybeResolvedLabel
resolveLabelAbs r@(ResolvedLabel _) = pure r
resolveLabelAbs (UnresolvedLabel l) = do
    offset <- gets @ConvertState (Map.lookup l . (.labelOffsets))
    case offset of
        Just o -> pure (ResolvedLabel o)
        Nothing -> pure (UnresolvedLabel l)

-- | Attempt to resolve a label to an __absolute__ offset, throwing an error if it cannot be resolved
mustBeResolvedAbs :: CodeConverterEff r => MaybeResolvedLabel -> Eff r Word16
mustBeResolvedAbs (ResolvedLabel i) = pure i
mustBeResolvedAbs (UnresolvedLabel l) = throwError (UnmarkedLabel l)

mustBeResolved :: CodeConverterEff r => Word16 -> MaybeResolvedLabel -> Eff r Word16
mustBeResolved instOffset = fmap (- instOffset) . mustBeResolvedAbs

-- | Convert an instruction with a real offset to a raw 'Raw.Instruction'.
convertInstruction :: CodeConverterEff r => OffsetInstruction (Abs.Instruction' MaybeResolvedLabel) -> Eff r (Maybe Raw.Instruction)
convertInstruction (OffsetInstruction _ (Abs.Label _)) = pure Nothing
convertInstruction (LabelInstruction _) = pure Nothing -- kind of awkward having 2 cases for the same thing here but what can you do
convertInstruction (OffsetInstruction instOffset o) = Just <$> convertInstruction o
  where
    convertInstruction (Abs.ALoad 0) = pure Raw.ALoad0
    convertInstruction (Abs.ALoad 1) = pure Raw.ALoad1
    convertInstruction (Abs.ALoad 2) = pure Raw.ALoad2
    convertInstruction (Abs.ALoad 3) = pure Raw.ALoad3
    convertInstruction (Abs.ALoad idx)
        | Right i <- tryInto @U1 idx = pure (Raw.ALoad i)
        | otherwise = pure (Raw.Wide1 MagicNumbers.instruction_aLoad idx)
    convertInstruction (Abs.AStore 0) = pure Raw.AStore0
    convertInstruction (Abs.AStore 1) = pure Raw.AStore1
    convertInstruction (Abs.AStore 2) = pure Raw.AStore2
    convertInstruction (Abs.AStore 3) = pure Raw.AStore3
    convertInstruction (Abs.AStore idx)
        | Right i <- tryInto @U1 idx = pure (Raw.AStore i)
        | otherwise = pure (Raw.Wide1 MagicNumbers.instruction_aStore idx)
    convertInstruction (Abs.ILoad 0) = pure Raw.ILoad0
    convertInstruction (Abs.ILoad 1) = pure Raw.ILoad1
    convertInstruction (Abs.ILoad 2) = pure Raw.ILoad2
    convertInstruction (Abs.ILoad 3) = pure Raw.ILoad3
    convertInstruction (Abs.ILoad idx)
        | Right i <- tryInto @U1 idx = pure (Raw.ILoad i)
        | otherwise = pure (Raw.Wide1 MagicNumbers.instruction_iLoad idx)
    convertInstruction (Abs.IStore 0) = pure Raw.IStore0
    convertInstruction (Abs.IStore 1) = pure Raw.IStore1
    convertInstruction (Abs.IStore 2) = pure Raw.IStore2
    convertInstruction (Abs.IStore 3) = pure Raw.IStore3
    convertInstruction (Abs.IStore idx)
        | Right i <- tryInto @U1 idx = pure (Raw.IStore i)
        | otherwise = pure (Raw.Wide1 MagicNumbers.instruction_iStore idx)
    convertInstruction Abs.ArrayLength = pure Raw.ArrayLength
    convertInstruction Abs.AALoad = pure Raw.AALoad
    convertInstruction Abs.AConstNull = pure Raw.AConstNull
    convertInstruction (Abs.Instanceof t) = do
        idx <- findIndexOf (CPClassEntry t)
        pure (Raw.Instanceof idx)
    convertInstruction (Abs.InvokeStatic c n m) = do
        idx <- findIndexOf (CPMethodRefEntry (MethodRef c n m))
        pure (Raw.InvokeStatic idx)
    convertInstruction (Abs.InvokeVirtual c n m) = do
        idx <- findIndexOf (CPMethodRefEntry (MethodRef c n m))
        pure (Raw.InvokeVirtual idx)
    convertInstruction (Abs.InvokeInterface c n m) = do
        idx <- findIndexOf (CPInterfaceMethodRefEntry (MethodRef c n m))
        let count = countArguments m
        pure (Raw.InvokeInterface idx (unsafeInto count))
    convertInstruction (Abs.InvokeSpecial c n m) = do
        idx <- findIndexOf (CPMethodRefEntry (MethodRef c n m))
        pure (Raw.InvokeSpecial idx)
    convertInstruction (Abs.LDC ldc) = do
        idx <-
            findIndexOf
                ( case ldc of
                    LDCInt i -> CPIntegerEntry i
                    LDCFloat f -> CPFloatEntry f
                    LDCString s -> CPStringEntry s
                    LDCClass c -> CPClassEntry c
                )

        pure (Raw.LDC_W idx) -- TODO: handle LDC vs LDC_W properly

    -- TODO: this should probably do a bounds check on the index
    convertInstruction (Abs.PutStatic c n t) = do
        idx <- findIndexOf (CPFieldRefEntry (FieldRef c n t))
        pure (Raw.PutStatic idx)
    convertInstruction (Abs.GetField c n t) = do
        idx <- findIndexOf (CPFieldRefEntry (FieldRef c n t))
        pure (Raw.GetField idx)
    convertInstruction (Abs.GetStatic c n t) = do
        idx <- findIndexOf (CPFieldRefEntry (FieldRef c n t))
        pure (Raw.GetStatic idx)
    convertInstruction (Abs.PutField c n t) = do
        idx <- findIndexOf (CPFieldRefEntry (FieldRef c n t))
        pure (Raw.PutField idx)
    convertInstruction Abs.AReturn = pure Raw.AReturn
    convertInstruction Abs.Return = pure Raw.Return
    convertInstruction Abs.IReturn = pure Raw.IReturn
    convertInstruction Abs.IConst0 = pure Raw.IConst0
    convertInstruction Abs.IConst1 = pure Raw.IConst1
    convertInstruction Abs.Dup = pure Raw.Dup
    convertInstruction (Abs.CheckCast t) = do
        idx <- findIndexOf (CPClassEntry t)
        pure (Raw.CheckCast idx)
    convertInstruction (Abs.InvokeDynamic bm n m) = do
        idx <- findIndexOf (CPInvokeDynamicEntry bm n m)
        pure (Raw.InvokeDynamic idx)
    convertInstruction (Abs.If cond) = do
        resolvedCond <- traverse (mustBeResolved instOffset) cond
        pure $ case resolvedCond of
            Abs.IfEq l -> Raw.IfEq l
            Abs.IfNe l -> Raw.IfNe l
            Abs.IfLt l -> Raw.IfLt l
            Abs.IfGe l -> Raw.IfGe l
            Abs.IfGt l -> Raw.IfGt l
            Abs.IfLe l -> Raw.IfLe l
    convertInstruction (Abs.Goto offset) = Raw.Goto <$> mustBeResolved instOffset offset
    convertInstruction (Abs.Label _) = bug "unreachable, should have already been handled"
    convertInstruction (Abs.New t) = do
        idx <- findIndexOf (CPClassEntry t)
        pure (Raw.New idx)
    convertInstruction Abs.IAnd = pure Raw.IAnd
    convertInstruction Abs.IOr = pure Raw.IOr
    convertInstruction (Abs.IfICmp cond) = do
        resolvedCond <- traverse (mustBeResolved instOffset) cond
        pure $ case resolvedCond of
            Abs.IfEq l -> Raw.IfIcmpEq l
            Abs.IfNe l -> Raw.IfIcmpNe l
            Abs.IfLt l -> Raw.IfIcmpLt l
            Abs.IfGe l -> Raw.IfIcmpGe l
            Abs.IfGt l -> Raw.IfIcmpGt l
            Abs.IfLe l -> Raw.IfIcmpLe l
    convertInstruction Abs.IAdd = pure Raw.IAdd
    convertInstruction Abs.ISub = pure Raw.ISub
    convertInstruction Abs.IMul = pure Raw.IMul
    convertInstruction Abs.IDiv = pure Raw.IDiv
