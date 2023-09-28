{-# LANGUAGE OverloadedRecordDot #-}

module JVM.Data.Convert.ConstantPool where

import Control.Lens (Lens', lens)
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.IndexedMap (IndexedMap)
import Data.IndexedMap qualified as IM
import Data.Text.Encoding
import Data.Vector (Vector)
import Data.Vector qualified as V
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Convert.Descriptor
import JVM.Data.Convert.Numbers
import JVM.Data.Convert.Type
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.ConstantPool
import JVM.Data.Raw.MagicNumbers
import JVM.Data.Raw.Types

lookupOrInsertM :: Monad m => ConstantPoolInfo -> ConstantPoolT m Int
lookupOrInsertM = IM.lookupOrInsertMOver _constantPool

_constantPool :: Lens' ConstantPoolState (IndexedMap ConstantPoolInfo)
_constantPool = lens constantPool (\s x -> s{constantPool = x})

transformEntry :: Monad m => ConstantPoolEntry -> ConstantPoolT m Int
transformEntry (CPUTF8Entry text) = lookupOrInsertM (UTF8Info $ encodeUtf8 text)
transformEntry (CPIntegerEntry i) = lookupOrInsertM (IntegerInfo $ fromIntegral i)
transformEntry (CPFloatEntry f) = lookupOrInsertM (FloatInfo (toJVMFloat f))
transformEntry (CPStringEntry msg) = do
    i <- transformEntry (CPUTF8Entry msg)
    lookupOrInsertM (StringInfo $ fromIntegral i)
transformEntry (CPLongEntry i) = do
    let (high, low) = toJVMLong i
    lookupOrInsertM (LongInfo high low)
transformEntry (CPDoubleEntry d) = do
    let (high, low) = toJVMLong (round d)
    lookupOrInsertM (DoubleInfo high low)
transformEntry (CPClassEntry name) = do
    let className = classInfoTypeDescriptor name
    nameIndex <- transformEntry (CPUTF8Entry className)
    lookupOrInsertM (ClassInfo $ fromIntegral nameIndex)
transformEntry (CPMethodRefEntry (MethodRef classRef name methodDescriptor)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (convertMethodDescriptor methodDescriptor))
    lookupOrInsertM (MethodRefInfo (fromIntegral classIndex) (fromIntegral nameAndTypeIndex))
transformEntry (CPNameAndTypeEntry name descriptor) = do
    nameIndex <- transformEntry (CPUTF8Entry name)
    descriptorIndex <- transformEntry (CPUTF8Entry descriptor)
    lookupOrInsertM (NameAndTypeInfo (fromIntegral nameIndex) (fromIntegral descriptorIndex))
transformEntry (CPFieldRefEntry (FieldRef classRef name fieldType)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (fieldTypeDescriptor fieldType))
    lookupOrInsertM (FieldRefInfo (fromIntegral classIndex) (fromIntegral nameAndTypeIndex))
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
    lookupOrInsertM (MethodHandleInfo referenceKind referenceIndex)
transformEntry (CPInvokeDynamicEntry bootstrapMethod name methodDescriptor) = do
    nameAndTypeIndex <- findIndexOf (CPNameAndTypeEntry name (convertMethodDescriptor methodDescriptor))
    bmIndex <- convertBootstrapMethod bootstrapMethod

    lookupOrInsertM (InvokeDynamicInfo (fromIntegral nameAndTypeIndex) (fromIntegral bmIndex))
transformEntry other = error $ "transformEntry: " <> show other

convertBootstrapMethod :: Monad m => BootstrapMethod -> ConstantPoolT m Int
convertBootstrapMethod (BootstrapMethod mhEntry args) = do
    mhIndex <- findIndexOf (CPMethodHandleEntry mhEntry)
    bsArgs <- traverse (findIndexOf . bmArgToCPEntry) args
    let bootstrapMethod = Raw.BootstrapMethod (fromIntegral mhIndex) (V.fromList bsArgs)
    IM.lookupOrInsertMOver (lens bootstrapMethods (\s x -> s{bootstrapMethods = x})) bootstrapMethod

data ConstantPoolState = ConstantPoolState
    { constantPool :: IndexedMap ConstantPoolInfo
    , bootstrapMethods :: IndexedMap Raw.BootstrapMethod
    }
    deriving (Show, Eq, Ord)

instance Semigroup ConstantPoolState where
    (ConstantPoolState cp1 bm1) <> (ConstantPoolState cp2 bm2) = ConstantPoolState (cp1 <> cp2) (bm1 <> bm2)

instance Monoid ConstantPoolState where
    mempty = ConstantPoolState mempty mempty

type ConstantPoolT m a = (StateT ConstantPoolState m) a

type ConstantPoolM a = ConstantPoolT Identity a

findIndexOf :: Monad m => ConstantPoolEntry -> ConstantPoolT m U2
findIndexOf = fmap toU2OrError . transformEntry
  where
    toU2OrError :: Int -> U2
    toU2OrError i =
        if i > fromIntegral (maxBound @U2)
            then error "Constant pool index out of bounds, too many entries?"
            else fromIntegral i

runConstantPoolM :: ConstantPoolM a -> (a, ConstantPoolState)
runConstantPoolM = runConstantPoolMWith mempty

runConstantPoolT :: Monad m => ConstantPoolT m a -> m (a, ConstantPoolState)
runConstantPoolT = runConstantPoolTWith mempty

runConstantPoolTWith :: Monad m => ConstantPoolState -> ConstantPoolT m a -> m (a, ConstantPoolState)
runConstantPoolTWith = flip runStateT

runConstantPoolMWith :: ConstantPoolState -> ConstantPoolM a -> (a, ConstantPoolState)
runConstantPoolMWith = runIdentity .: runConstantPoolTWith
  where
    (.:) = (.) . (.)
