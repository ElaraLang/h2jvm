{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module JVM.Data.Convert.ConstantPool (ConstantPool, ConstantPoolState (..), runConstantPool, runConstantPoolWith, findIndexOf) where

import Data.IndexedMap (IndexedMap)
import Data.IndexedMap qualified as IM
import Data.Text.Encoding
import Data.Vector qualified as V
import Effectful
import Effectful.State.Static.Local
import GHC.Stack
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Convert.Descriptor
import JVM.Data.Convert.Numbers
import JVM.Data.Convert.Type
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.ConstantPool
import JVM.Data.Raw.MagicNumbers
import JVM.Data.Raw.Types
import Witch
import Data.Word (Word32)

data ConstantPoolState = ConstantPoolState
  { constantPool :: IndexedMap ConstantPoolInfo,
    bootstrapMethods :: IndexedMap Raw.BootstrapMethod
  }
  deriving (Eq, Ord, Show)

lookupOrInsertMOver ::
  (ConstantPool :> r, Ord a) =>
  a ->
  (ConstantPoolState -> IndexedMap a) ->
  (ConstantPoolState -> IndexedMap a -> ConstantPoolState) ->
  Eff r Word32
lookupOrInsertMOver cpInfo getter setter = do
  cp <- get
  let (i, newCP) = IM.lookupOrInsert cpInfo (getter cp)
  put (setter cp newCP)
  pure $ IM.indexValue i

lookupOrInsertMCP :: (ConstantPool :> r) => ConstantPoolInfo -> Eff r Word32
lookupOrInsertMCP cpInfo = lookupOrInsertMOver cpInfo (.constantPool) (\s x -> s {constantPool = x})

lookupOrInsertMBM :: (ConstantPool :> r) => Raw.BootstrapMethod -> Eff r Word32
lookupOrInsertMBM bmInfo = lookupOrInsertMOver bmInfo (.bootstrapMethods) (\s x -> s {bootstrapMethods = x})

transformEntry :: (HasCallStack, ConstantPool :> r) => ConstantPoolEntry -> Eff r Word32
transformEntry (CPUTF8Entry text) = lookupOrInsertMCP (UTF8Info $ encodeUtf8 text)
transformEntry (CPIntegerEntry i) = lookupOrInsertMCP (IntegerInfo $ unsafeInto i)
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
  let transformFieldMHE f@(FieldRef {}) = findIndexOf (CPFieldRefEntry f)
  let transformMethodMHE m@(MethodRef {}) = findIndexOf (CPMethodRefEntry m)

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

convertBootstrapMethod :: (ConstantPool :> r) => BootstrapMethod -> Eff r Word32
convertBootstrapMethod (BootstrapMethod mhEntry args) = do
  mhIndex <- findIndexOf (CPMethodHandleEntry mhEntry)
  bsArgs <- traverse (findIndexOf . bmArgToCPEntry) args
  let bootstrapMethod = Raw.BootstrapMethod (into mhIndex) (V.fromList bsArgs)
  lookupOrInsertMBM bootstrapMethod

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

type ConstantPool = State ConstantPoolState

runConstantPoolWith :: ConstantPoolState -> Eff (ConstantPool ': r) a -> Eff r (a, ConstantPoolState)
runConstantPoolWith s =
  runState s
    . inject

runConstantPool :: Eff (ConstantPool ': r) a -> Eff r (a, ConstantPoolState)
runConstantPool = runConstantPoolWith mempty
