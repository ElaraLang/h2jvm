{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Conversion of the high level constant pool types to the low level representation.
This uses the 'H2JVM.Internal.IndexedMap.IndexedMap' type to keep the constant pool free from duplication.
-}
module H2JVM.Internal.Convert.ConstantPool (ConstantPool, ConstantPoolState (..), runConstantPool, runConstantPoolWith, findIndexOf) where

import Data.Text.Encoding
import Data.Word (Word32)
import Effectful
import Effectful.State.Static.Local
import GHC.Stack
import Witch

import Data.Vector qualified as V

import H2JVM.ConstantPool
import H2JVM.Internal.Convert.Numbers
import H2JVM.Internal.Convert.Type
import H2JVM.Internal.IndexedMap (IndexedMap)
import H2JVM.Internal.Raw.ConstantPool
import H2JVM.Internal.Raw.MagicNumbers
import H2JVM.Internal.Raw.Types

import H2JVM.Internal.IndexedMap qualified as IM
import H2JVM.Internal.Raw.ClassFile qualified as Raw

-- | Accumulated state of the constant pool.
data ConstantPoolState = ConstantPoolState
    { constantPool :: IndexedMap ConstantPoolInfo
    -- ^ A bidirectional mapping from index to a 'ConstantPoolInfo'.
    , bootstrapMethods :: IndexedMap Raw.BootstrapMethod
    -- ^ A bidirectional mapping from an index to a 'Raw.BootstrapMethod'.
    }
    deriving (Eq, Ord, Show)

-- | A simple state monad for accumulating a 'ConstantPoolState'
type ConstantPool = State ConstantPoolState

runConstantPoolWith :: ConstantPoolState -> Eff (ConstantPool ': r) a -> Eff r (a, ConstantPoolState)
runConstantPoolWith s =
    runState s
        . inject

-- | Run a 'ConstantPool' effect with an empty initial state.
runConstantPool :: Eff (ConstantPool ': r) a -> Eff r (a, ConstantPoolState)
runConstantPool = runConstantPoolWith mempty

{- | Insert or lookup a value within one of the fields of 'ConstantPoolState'.
Essentially a wrapper of 'IM.lookupOrInsert' over some getter and setter function.
-}
lookupOrInsertMOver ::
    (ConstantPool :> r, Ord a) =>
    a ->
    -- | Getter function.
    (ConstantPoolState -> IndexedMap a) ->
    -- | Setter function.
    (ConstantPoolState -> IndexedMap a -> ConstantPoolState) ->
    -- | Index into the constant pool.
    Eff r Word32
lookupOrInsertMOver cpInfo getter setter = do
    cp <- get
    let (i, newCP) = IM.lookupOrInsert cpInfo (getter cp)
    put (setter cp newCP)
    pure $ IM.indexValue i

-- | Insert or lookup a value within the 'constantPool' field of 'ConstantPoolState'.
lookupOrInsertMCP :: ConstantPool :> r => ConstantPoolInfo -> Eff r Word32
lookupOrInsertMCP cpInfo = lookupOrInsertMOver cpInfo (.constantPool) (\s x -> s{constantPool = x})

-- | Insert or lookup a value within the 'bootstrapMethods' field of 'ConstantPoolState'.
lookupOrInsertMBM :: ConstantPool :> r => Raw.BootstrapMethod -> Eff r Word32
lookupOrInsertMBM bmInfo = lookupOrInsertMOver bmInfo (.bootstrapMethods) (\s x -> s{bootstrapMethods = x})

{- | Transform a single, high-level 'ConstantPoolEntry' into a raw constant pool entry by upserting it into the 'ConstantPoolState'.
Returns the index of the upserted entry.
A little bit partial in the case of invalid/overflowing indices, which should be impossible in most normal cases.
-}
transformEntry :: (HasCallStack, ConstantPool :> r) => ConstantPoolEntry -> Eff r Word32
transformEntry (CPUTF8Entry text) = lookupOrInsertMCP (UTF8Info $ encodeUtf8 text)
transformEntry (CPIntegerEntry i) = lookupOrInsertMCP (IntegerInfo $ fromIntegral i)
transformEntry (CPFloatEntry f) = lookupOrInsertMCP (FloatInfo (toJVMFloat f))
transformEntry (CPStringEntry msg) = do
    i <- transformEntry (CPUTF8Entry msg)
    lookupOrInsertMCP (StringInfo $ unsafeInto i)
transformEntry (CPLongEntry i) = do
    let (high, low) = toJVMLong i
    lookupOrInsertMCP (LongInfo high low)
transformEntry (CPDoubleEntry d) = do
    let (high, low) = toJVMLong (round d)
    lookupOrInsertMCP (DoubleInfo high low)
transformEntry (CPClassEntry name) = do
    let className = classInfoTypeDescriptor name
    nameIndex <- transformEntry (CPUTF8Entry className)
    lookupOrInsertMCP (ClassInfo $ unsafeInto nameIndex)
transformEntry (CPMethodRefEntry (MethodRef classRef name methodDescriptor)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (convertMethodDescriptor methodDescriptor))
    lookupOrInsertMCP (MethodRefInfo (unsafeInto classIndex) (unsafeInto nameAndTypeIndex))
transformEntry (CPInterfaceMethodRefEntry (MethodRef classRef name methodDescriptor)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (convertMethodDescriptor methodDescriptor))
    lookupOrInsertMCP (InterfaceMethodRefInfo (unsafeInto classIndex) (unsafeInto nameAndTypeIndex))
transformEntry (CPNameAndTypeEntry name descriptor) = do
    nameIndex <- transformEntry (CPUTF8Entry name)
    descriptorIndex <- transformEntry (CPUTF8Entry descriptor)
    lookupOrInsertMCP (NameAndTypeInfo (unsafeInto nameIndex) (unsafeInto descriptorIndex))
transformEntry (CPFieldRefEntry (FieldRef classRef name fieldType)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (fieldTypeDescriptor fieldType))
    lookupOrInsertMCP (FieldRefInfo (unsafeInto classIndex) (unsafeInto nameAndTypeIndex))
transformEntry (CPMethodHandleEntry methodHandleEntry) = do
    let transformFieldMHE f@(FieldRef{}) = findIndexOf (CPFieldRefEntry f)
    let transformMethodMHE m@(MethodRef{}) = findIndexOf (CPMethodRefEntry m)

    (referenceKind, referenceIndex) <- case methodHandleEntry of
        MHGetField fr -> do
            fri <- transformFieldMHE fr
            pure (_REF_getField, fri)
        MHGetStatic fr -> do
            fri <- transformFieldMHE fr
            pure (_REF_getStatic, fri)
        MHPutField fr -> do
            fri <- transformFieldMHE fr
            pure (_REF_putField, fri)
        MHPutStatic fr -> do
            fri <- transformFieldMHE fr
            pure (_REF_putStatic, fri)
        MHInvokeVirtual mr -> do
            mri <- transformMethodMHE mr
            pure (_REF_invokeVirtual, mri)
        MHNewInvokeSpecial mr -> do
            mri <- transformMethodMHE mr
            pure (_REF_newInvokeSpecial, mri)
        MHInvokeStatic mr -> do
            mri <- transformMethodMHE mr
            pure (_REF_invokeStatic, mri)
        MHInvokeSpecial mr -> do
            mri <- transformMethodMHE mr
            pure (_REF_invokeSpecial, mri)
        MHInvokeInterface mr -> do
            mri <- transformMethodMHE mr
            pure (_REF_invokeInterface, mri)
    lookupOrInsertMCP (MethodHandleInfo referenceKind referenceIndex)
transformEntry (CPInvokeDynamicEntry bootstrapMethod name methodDescriptor) = do
    nameAndTypeIndex <- findIndexOf (CPNameAndTypeEntry name (convertMethodDescriptor methodDescriptor))
    bmIndex <- convertBootstrapMethod bootstrapMethod
    if bmIndex == 0 -- bootstrap methods are 0 indexed because of course they are
        then error "Invalid Bootstrap Method Index 0"
        else
            lookupOrInsertMCP
                ( InvokeDynamicInfo
                    (unsafeInto bmIndex - 1)
                    (into nameAndTypeIndex)
                )
transformEntry (CPMethodTypeEntry methodDescriptor) = do
    descriptorIndex <- transformEntry (CPUTF8Entry (convertMethodDescriptor methodDescriptor))
    lookupOrInsertMCP (MethodTypeInfo (unsafeInto descriptorIndex))

-- | Convert a 'BootstrapMethod' to its low level representation by upserting it into the 'ConstantPoolState'.
convertBootstrapMethod :: ConstantPool :> r => BootstrapMethod -> Eff r Word32
convertBootstrapMethod (BootstrapMethod mhEntry args) = do
    mhIndex <- findIndexOf (CPMethodHandleEntry mhEntry) -- upsert a method handle entry to the CP
    bsArgs <- traverse (findIndexOf . bmArgToCPEntry) args -- upsert each argument into the CP
    let bootstrapMethod = Raw.BootstrapMethod (into mhIndex) (V.fromList bsArgs) -- construct raw BM
    lookupOrInsertMBM bootstrapMethod -- upsert BM

instance Semigroup ConstantPoolState where
    (ConstantPoolState cp1 bm1) <> (ConstantPoolState cp2 bm2) = ConstantPoolState (cp1 <> cp2) (bm1 <> bm2)

instance Monoid ConstantPoolState where
    mempty = ConstantPoolState mempty mempty

-- | Find the index of a constant pool entry, inserting it in the first free location if it doesn't exist.
findIndexOf :: (HasCallStack, ConstantPool :> r) => ConstantPoolEntry -> Eff r U2
findIndexOf = fmap toU2OrError . transformEntry
  where
    toU2OrError :: Word32 -> U2
    toU2OrError i =
        if i > into (maxBound @U2)
            then error "Constant pool index out of bounds, too many entries?"
            else unsafeInto i
