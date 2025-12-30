{-# LANGUAGE LexicalNegation #-}

{- | Converts abstract instructions into raw instructions
 This includes resolving labels into offsets
 TODO: this is very inefficient, requiring three passes over the instructions
-}
module JVM.Data.Convert.Instruction (CodeConverterEff, fullyRunCodeConverter, convertInstructions, fullyResolveAbs) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..), FieldRef (..), MethodRef (..))
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction as Abs (Instruction, Instruction' (..), LDCEntry (..))
import JVM.Data.Abstract.Type
import JVM.Data.Convert.ConstantPool
import JVM.Data.Raw.Instruction as Raw (Instruction (..))

import Control.Monad (void)
import Data.Word (Word16)
import Debug.Trace (traceM)
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import JVM.Data.Convert.Monad
import JVM.Data.Raw.MagicNumbers qualified as MagicNumbers
import JVM.Data.Raw.Types (U1, U2, UnsafeNumConvert (unsafeNumConvert), safeNumConvert)

type CodeConverterEff r = (ConstantPool :> r, State ConvertState :> r, Error CodeConverterError :> r)

fullyRunCodeConverter :: (ConvertEff r') => Eff (State ConvertState : r') a -> Eff r' a
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
    countArgument (PrimitiveFieldType Double) = 2
    countArgument (PrimitiveFieldType Long) = 2
    countArgument _ = 1

-- | The size of an instruction in bytes, used for calculating jump offsets
instructionSize :: Abs.Instruction -> Word16
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
instructionSize Abs.AConstNull = 1
instructionSize (Abs.IfEq _) = 3
instructionSize (Abs.IfNe _) = 3
instructionSize (Abs.IfLt _) = 3
instructionSize (Abs.IfGe _) = 3
instructionSize (Abs.IfGt _) = 3
instructionSize (Abs.IfLe _) = 3
instructionSize (Abs.Instanceof{}) = 3
instructionSize (Abs.InvokeStatic{}) = 3
instructionSize (Abs.InvokeVirtual{}) = 3
instructionSize (Abs.InvokeInterface{}) = 5
instructionSize (Abs.InvokeDynamic{}) = 5
instructionSize (Abs.InvokeSpecial{}) = 3
instructionSize (Abs.Label _) = 0
instructionSize (Abs.LDC _) = 3
instructionSize (Abs.PutStatic{}) = 3
instructionSize (Abs.GetField{}) = 3
instructionSize (Abs.GetStatic{}) = 3
instructionSize (Abs.PutField{}) = 3
instructionSize (Abs.CheckCast _) = 3
instructionSize Abs.Return = 1
instructionSize Abs.Dup = 1
instructionSize (Abs.Goto _) = 3
instructionSize (Abs.New _) = 3

convertInstructions :: (CodeConverterEff r) => [Abs.Instruction] -> Eff r [Raw.Instruction]
convertInstructions xs = do
    withOffsets <- insertAllLabels xs
    insts <- traverse (resolveLabel . fmap (fmap UnresolvedLabel)) withOffsets
    catMaybes <$> traverse convertInstruction insts

data MaybeResolvedLabel = ResolvedLabel Word16 | UnresolvedLabel Label

data OffsetInstruction a = OffsetInstruction Word16 a
    deriving (Show, Eq, Ord, Functor)

-- | Inserts the corresponding label offsets into the state
insertAllLabels :: (CodeConverterEff r) => [Abs.Instruction] -> Eff r [OffsetInstruction Abs.Instruction]
insertAllLabels = traverse (\x -> incOffset x *> insertLabel x)
  where
    incOffset :: (CodeConverterEff r) => Abs.Instruction -> Eff r ()
    incOffset (Label _) = pure () -- Label instructions have no representation in the bytecode, so they don't affect the offset
    incOffset inst = do
        offset <- gets (.currentOffset)
        let size = instructionSize inst
        traceM $ "Offset: " ++ show offset ++ " | Size: " ++ show size ++ " | Inst: " ++ show (void inst)
        modify (\s -> s{currentOffset = offset + size})

    insertLabel :: (CodeConverterEff r) => Abs.Instruction -> Eff r (OffsetInstruction Abs.Instruction)
    insertLabel (Label l) = do
        currentOffset <- gets (.currentOffset)

        JVM.Data.Convert.Instruction.modifyM $ \s -> do
            case Map.lookup l s.labelOffsets of
                Just _ -> throwError (DuplicateLabel l currentOffset)
                Nothing -> do
                    pure (s{labelOffsets = Map.insert l currentOffset s.labelOffsets})
        pure (OffsetInstruction (error "Label offset should not be evaluated") (Label l))
    insertLabel x = do
        offset <- gets (.currentOffset)
        pure (OffsetInstruction (offset - instructionSize x) x)

modifyM :: ((State s) :> r) => (s -> Eff r s) -> Eff r ()
modifyM f = get >>= f >>= put

-- | Turns labels into offsets where possible
resolveLabel :: (CodeConverterEff r) => OffsetInstruction (Abs.Instruction' MaybeResolvedLabel) -> Eff r (OffsetInstruction (Abs.Instruction' MaybeResolvedLabel))
resolveLabel (OffsetInstruction instOffset inst) =
    OffsetInstruction instOffset <$> case inst of
        Abs.IfEq l -> Abs.IfEq <$> resolveLabelAbs l
        Abs.IfNe l -> Abs.IfNe <$> resolveLabelAbs l
        Abs.IfLt l -> Abs.IfLt <$> resolveLabelAbs l
        Abs.IfGe l -> Abs.IfGe <$> resolveLabelAbs l
        Abs.IfGt l -> Abs.IfGt <$> resolveLabelAbs l
        Abs.IfLe l -> Abs.IfLe <$> resolveLabelAbs l
        Abs.Goto l -> Abs.Goto <$> resolveLabelAbs l
        _ -> pure inst

fullyResolveAbs :: (CodeConverterEff r) => Label -> Eff r Word16
fullyResolveAbs l = do
    x <- resolveLabelAbs (UnresolvedLabel l)
    mustBeResolvedAbs x

-- | Attempt to resolve a label to an __absolute__ offset
resolveLabelAbs :: (CodeConverterEff r) => MaybeResolvedLabel -> Eff r MaybeResolvedLabel
resolveLabelAbs r@(ResolvedLabel _) = pure r
resolveLabelAbs (UnresolvedLabel l) = do
    offset <- gets (Map.lookup l . (.labelOffsets))
    case offset of
        Just o -> pure (ResolvedLabel o)
        Nothing -> pure (UnresolvedLabel l)

-- | Attempt to resolve a label to an __absolute__ offset, throwing an error if it cannot be resolved
mustBeResolvedAbs :: (CodeConverterEff r) => MaybeResolvedLabel -> Eff r Word16
mustBeResolvedAbs (ResolvedLabel i) = pure i
mustBeResolvedAbs (UnresolvedLabel l) = throwError (UnmarkedLabel l)

mustBeResolved :: (CodeConverterEff r) => Word16 -> MaybeResolvedLabel -> Eff r Word16
mustBeResolved instOffset = fmap (- instOffset) . mustBeResolvedAbs

convertInstruction :: (CodeConverterEff r) => OffsetInstruction (Abs.Instruction' MaybeResolvedLabel) -> Eff r (Maybe Raw.Instruction)
convertInstruction (OffsetInstruction _ (Abs.Label _)) = pure Nothing
convertInstruction (OffsetInstruction instOffset o) = Just <$> convertInstruction o
  where
    convertInstruction (Abs.ALoad 0) = pure Raw.ALoad0
    convertInstruction (Abs.ALoad 1) = pure Raw.ALoad1
    convertInstruction (Abs.ALoad 2) = pure Raw.ALoad2
    convertInstruction (Abs.ALoad 3) = pure Raw.ALoad3
    convertInstruction (Abs.ALoad idx)
        | Just i <- unsafeNumConvert @U2 @U1 idx = pure (Raw.ALoad i)
        | otherwise = pure (Raw.Wide1 MagicNumbers.instruction_aLoad idx)
    convertInstruction (Abs.AStore 0) = pure Raw.AStore0
    convertInstruction (Abs.AStore 1) = pure Raw.AStore1
    convertInstruction (Abs.AStore 2) = pure Raw.AStore2
    convertInstruction (Abs.AStore 3) = pure Raw.AStore3
    convertInstruction (Abs.AStore idx)
        | Just i <- unsafeNumConvert @U2 @U1 idx = pure (Raw.AStore i)
        | otherwise = pure (Raw.Wide1 MagicNumbers.instruction_aStore idx)
    convertInstruction (Abs.ILoad 0) = pure Raw.ILoad0
    convertInstruction (Abs.ILoad 1) = pure Raw.ILoad1
    convertInstruction (Abs.ILoad 2) = pure Raw.ILoad2
    convertInstruction (Abs.ILoad 3) = pure Raw.ILoad3
    convertInstruction (Abs.ILoad idx)
        | Just i <- unsafeNumConvert @U2 @U1 idx = pure (Raw.ILoad i)
        | otherwise = pure (Raw.Wide1 MagicNumbers.instruction_iLoad idx)
    convertInstruction (Abs.IStore 0) = pure Raw.IStore0
    convertInstruction (Abs.IStore 1) = pure Raw.IStore1
    convertInstruction (Abs.IStore 2) = pure Raw.IStore2
    convertInstruction (Abs.IStore 3) = pure Raw.IStore3
    convertInstruction (Abs.IStore idx)
        | Just i <- unsafeNumConvert @U2 @U1 idx = pure (Raw.IStore i)
        | otherwise = pure (Raw.Wide1 MagicNumbers.instruction_iStore idx)
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
        pure (Raw.InvokeInterface idx (fromIntegral count))
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
    convertInstruction Abs.Dup = pure Raw.Dup
    convertInstruction (Abs.CheckCast t) = do
        idx <- findIndexOf (CPClassEntry t)
        pure (Raw.CheckCast idx)
    convertInstruction (Abs.InvokeDynamic bm n m) = do
        idx <- findIndexOf (CPInvokeDynamicEntry bm n m)
        pure (Raw.InvokeDynamic idx)
    convertInstruction (Abs.IfEq offset) = Raw.IfEq <$> mustBeResolved instOffset offset
    convertInstruction (Abs.IfNe offset) = Raw.IfNe <$> mustBeResolved instOffset offset
    convertInstruction (Abs.IfLt offset) = Raw.IfLt <$> mustBeResolved instOffset offset
    convertInstruction (Abs.IfGe offset) = Raw.IfGe <$> mustBeResolved instOffset offset
    convertInstruction (Abs.IfGt offset) = Raw.IfGt <$> mustBeResolved instOffset offset
    convertInstruction (Abs.IfLe offset) = Raw.IfLe <$> mustBeResolved instOffset offset
    convertInstruction (Abs.Goto offset) = Raw.Goto <$> mustBeResolved instOffset offset
    convertInstruction (Abs.Label _) = error "unreachable"
    convertInstruction (Abs.New t) = do
        idx <- findIndexOf (CPClassEntry t)
        pure (Raw.New idx)
