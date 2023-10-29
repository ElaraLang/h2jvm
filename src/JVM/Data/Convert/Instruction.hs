-- | Converts abstract instructions into raw instructions
-- This includes resolving labels into offsets
-- TODO: this is very inefficient, requiring three passes over the instructions
module JVM.Data.Convert.Instruction (fullyRunCodeConverter, convertInstructions) where

import Control.Monad.Except
import Control.Monad.State
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

import Data.Word (Word16)
import Debug.Trace (traceM)
import JVM.Data.Convert.Monad (CodeConverterError (..), ConvertM)

newtype CodeConverter a = CodeConverter {runCodeConverter :: (StateT ConvertState (ConstantPoolT (Except CodeConverterError))) a}
    deriving (Functor, Applicative, Monad, MonadState ConvertState)

instance MonadConstantPool CodeConverter where
    findIndexOf = CodeConverter . lift . findIndexOf

fullyRunCodeConverter :: CodeConverter a -> ConvertM a
fullyRunCodeConverter xs = do
    (a, _) <- runStateT (runCodeConverter xs) (ConvertState 0 Map.empty)
    pure a

instance MonadError CodeConverterError CodeConverter where
    throwError = CodeConverter . lift . throwError
    catchError (CodeConverter m) f = CodeConverter (catchError m (runCodeConverter . f))

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
instructionSize Abs.ALoad0 = 1
instructionSize Abs.ALoad1 = 1
instructionSize Abs.ALoad2 = 1
instructionSize Abs.ALoad3 = 1
instructionSize (Abs.ALoad _) = 2
instructionSize Abs.AStore0 = 1
instructionSize Abs.AStore1 = 1
instructionSize Abs.AStore2 = 1
instructionSize Abs.AStore3 = 1
instructionSize (Abs.AStore _) = 2
instructionSize Abs.AReturn = 1
instructionSize Abs.AThrow = 1
instructionSize Abs.AConstNull = 1
instructionSize (Abs.IfEq _) = 3
instructionSize (Abs.IfNe _) = 3
instructionSize (Abs.IfLt _) = 3
instructionSize (Abs.IfGe _) = 3
instructionSize (Abs.IfGt _) = 3
instructionSize (Abs.IfLe _) = 3
instructionSize (Abs.InvokeStatic{}) = 3
instructionSize (Abs.InvokeVirtual{}) = 3
instructionSize (Abs.InvokeInterface{}) = 5
instructionSize (Abs.InvokeDynamic{}) = 5
instructionSize (Abs.Label _) = 0
instructionSize (Abs.LDC _) = 2
instructionSize (Abs.PutStatic{}) = 3
instructionSize (Abs.GetStatic{}) = 3
instructionSize (Abs.CheckCast _) = 3
instructionSize Abs.Return = 1
instructionSize (Abs.Goto _) = 3

convertInstructions :: [Abs.Instruction] -> CodeConverter [Raw.Instruction]
convertInstructions xs = do
    withOffsets <- insertAllLabels xs
    insts <- traverse resolveLabel (fmap (fmap UnresolvedLabel) <$> withOffsets)
    catMaybes <$> traverse convertInstruction insts

data MaybeResolvedLabel = ResolvedLabel Word16 | UnresolvedLabel Label

data OffsetInstruction a = OffsetInstruction Word16 a
    deriving (Show, Eq, Ord, Functor)

-- | Inserts the corresponding label offsets into the state
insertAllLabels :: [Abs.Instruction] -> CodeConverter [OffsetInstruction Abs.Instruction]
insertAllLabels = traverse (\x -> incOffset x *> insertLabel x)
  where
    incOffset :: Abs.Instruction -> CodeConverter ()
    incOffset (Label _) = pure () -- Label instructions have no representation in the bytecode, so they don't affect the offset
    incOffset inst = do
        offset <- gets currentOffset
        modify (\s -> s{currentOffset = offset + instructionSize inst})

    insertLabel :: Abs.Instruction -> CodeConverter (OffsetInstruction Abs.Instruction)
    insertLabel (Label l) = do
        currentOffset <- gets currentOffset

        CodeConverter $ modifyM $ \s -> do
            case Map.lookup l (labelOffsets s) of
                Just _ -> throwError (DuplicateLabel l currentOffset)
                Nothing -> do
                    pure (s{labelOffsets = Map.insert l currentOffset (labelOffsets s)})
        pure (OffsetInstruction (error "Label offset should not be evaluated") (Label l))
    insertLabel x = do
        offset <- gets currentOffset
        pure (OffsetInstruction (offset - instructionSize x) x)

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ \s -> do
    s' <- f s
    return ((), s')

-- | Turns labels into offsets where possible
resolveLabel :: OffsetInstruction (Abs.Instruction' MaybeResolvedLabel) -> CodeConverter (OffsetInstruction (Abs.Instruction' MaybeResolvedLabel))
resolveLabel (OffsetInstruction instOffset inst) =
    let resolveLabel' :: MaybeResolvedLabel -> CodeConverter MaybeResolvedLabel
        resolveLabel' r@(ResolvedLabel _) = pure r
        resolveLabel' (UnresolvedLabel l) = do
            offset <- gets (Map.lookup l . labelOffsets)
            case offset of
                Just o -> pure (ResolvedLabel o)
                Nothing -> pure (UnresolvedLabel l)
     in OffsetInstruction instOffset <$> case inst of
            Abs.IfEq l -> Abs.IfEq <$> resolveLabel' l
            Abs.IfNe l -> Abs.IfNe <$> resolveLabel' l
            Abs.IfLt l -> Abs.IfLt <$> resolveLabel' l
            Abs.IfGe l -> Abs.IfGe <$> resolveLabel' l
            Abs.IfGt l -> Abs.IfGt <$> resolveLabel' l
            Abs.IfLe l -> Abs.IfLe <$> resolveLabel' l
            Abs.Goto l -> Abs.Goto <$> resolveLabel' l
            _ -> pure inst

mustBeResolved :: Word16 -> MaybeResolvedLabel -> CodeConverter Word16
mustBeResolved relativeTo (ResolvedLabel i) = pure (i - relativeTo)
mustBeResolved _ (UnresolvedLabel l) = throwError (UnmarkedLabel l)

convertInstruction :: OffsetInstruction (Abs.Instruction' MaybeResolvedLabel) -> CodeConverter (Maybe Raw.Instruction)
convertInstruction (OffsetInstruction _ (Abs.Label _)) = pure Nothing
convertInstruction (OffsetInstruction instOffset o) = Just <$> convertInstruction o
  where
    convertInstruction Abs.ALoad0 = pure Raw.ALoad0
    convertInstruction Abs.ALoad1 = pure Raw.ALoad1
    convertInstruction Abs.ALoad2 = pure Raw.ALoad2
    convertInstruction Abs.ALoad3 = pure Raw.ALoad3
    convertInstruction (Abs.ALoad 0) = pure Raw.ALoad0
    convertInstruction (Abs.ALoad 1) = pure Raw.ALoad1
    convertInstruction (Abs.ALoad 2) = pure Raw.ALoad2
    convertInstruction (Abs.ALoad 3) = pure Raw.ALoad3
    convertInstruction (Abs.ALoad idx) = pure (Raw.ALoad idx)
    convertInstruction Abs.AStore0 = pure Raw.AStore0
    convertInstruction Abs.AStore1 = pure Raw.AStore1
    convertInstruction Abs.AStore2 = pure Raw.AStore2
    convertInstruction Abs.AStore3 = pure Raw.AStore3
    convertInstruction (Abs.AStore 0) = pure Raw.AStore0
    convertInstruction (Abs.AStore 1) = pure Raw.AStore1
    convertInstruction (Abs.AStore 2) = pure Raw.AStore2
    convertInstruction (Abs.AStore 3) = pure Raw.AStore3
    convertInstruction (Abs.AStore idx) = pure (Raw.AStore idx)
    convertInstruction Abs.AThrow = pure Raw.AThrow
    convertInstruction Abs.AConstNull = pure Raw.AConstNull
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
    convertInstruction (Abs.LDC ldc) = do
        idx <-
            findIndexOf
                ( case ldc of
                    LDCInt i -> CPIntegerEntry i
                    LDCFloat f -> CPFloatEntry f
                    LDCString s -> CPStringEntry s
                    LDCClass c -> CPClassEntry c
                )

        pure (Raw.LDC (fromIntegral idx)) -- for some reason, the index is a u8, not a u16
        -- TODO: this should probably do a bounds check on the index
    convertInstruction (Abs.PutStatic c n t) = do
        idx <- findIndexOf (CPFieldRefEntry (FieldRef c n t))
        pure (Raw.PutStatic idx)
    convertInstruction (Abs.GetStatic c n t) = do
        idx <- findIndexOf (CPFieldRefEntry (FieldRef c n t))
        pure (Raw.GetStatic idx)
    convertInstruction Abs.AReturn = pure Raw.AReturn
    convertInstruction Abs.Return = pure Raw.Return
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
