{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module JVM.Data.Convert.ConstantPool where

import Data.IndexedMap (IndexedMap)
import Data.IndexedMap qualified as IM
import Data.Text.Encoding
import Data.Tuple (swap)
import Data.Vector qualified as V
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Convert.Descriptor
import JVM.Data.Convert.Numbers
import JVM.Data.Convert.Type
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.ConstantPool
import JVM.Data.Raw.MagicNumbers
import JVM.Data.Raw.Types
import Polysemy
import Polysemy.State

data ConstantPoolState = ConstantPoolState
    { constantPool :: IndexedMap ConstantPoolInfo
    , bootstrapMethods :: IndexedMap Raw.BootstrapMethod
    }
    deriving (Show, Eq, Ord)

data ConstantPool m a where
    GetCP :: ConstantPool m ConstantPoolState
    SetCP :: ConstantPoolState -> ConstantPool m ()

makeSem ''ConstantPool

lookupOrInsertMOver :: (Member ConstantPool r, Ord a) => a -> (ConstantPoolState -> IndexedMap a) -> (ConstantPoolState -> IndexedMap a -> ConstantPoolState) -> Sem r Int
lookupOrInsertMOver cpInfo get set = do
    cp <- getCP
    let (i, newCP) = IM.lookupOrInsert cpInfo (get cp)
    setCP (set cp newCP)
    pure i

lookupOrInsertMCP :: (Member ConstantPool r) => ConstantPoolInfo -> Sem r Int
lookupOrInsertMCP cpInfo = lookupOrInsertMOver cpInfo (.constantPool) (\s x -> s{constantPool = x})
lookupOrInsertMBM :: (Member ConstantPool r) => Raw.BootstrapMethod -> Sem r Int
lookupOrInsertMBM bmInfo = lookupOrInsertMOver bmInfo (.bootstrapMethods) (\s x -> s{bootstrapMethods = x})

transformEntry :: (Member ConstantPool r) => ConstantPoolEntry -> Sem r Int
transformEntry (CPUTF8Entry text) = lookupOrInsertMCP (UTF8Info $ encodeUtf8 text)
transformEntry (CPIntegerEntry i) = lookupOrInsertMCP (IntegerInfo $ fromIntegral i)
transformEntry (CPFloatEntry f) = lookupOrInsertMCP (FloatInfo (toJVMFloat f))
transformEntry (CPStringEntry msg) = do
    i <- transformEntry (CPUTF8Entry msg)
    lookupOrInsertMCP (StringInfo $ fromIntegral i)
transformEntry (CPLongEntry i) = do
    let (high, low) = toJVMLong i
    lookupOrInsertMCP (LongInfo high low)
transformEntry (CPDoubleEntry d) = do
    let (high, low) = toJVMLong (round d)
    lookupOrInsertMCP (DoubleInfo high low)
transformEntry (CPClassEntry name) = do
    let className = classInfoTypeDescriptor name
    nameIndex <- transformEntry (CPUTF8Entry className)
    lookupOrInsertMCP (ClassInfo $ fromIntegral nameIndex)
transformEntry (CPMethodRefEntry (MethodRef classRef name methodDescriptor)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (convertMethodDescriptor methodDescriptor))
    lookupOrInsertMCP (MethodRefInfo (fromIntegral classIndex) (fromIntegral nameAndTypeIndex))
transformEntry (CPInterfaceMethodRefEntry (MethodRef classRef name methodDescriptor)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (convertMethodDescriptor methodDescriptor))
    lookupOrInsertMCP (InterfaceMethodRefInfo (fromIntegral classIndex) (fromIntegral nameAndTypeIndex))
transformEntry (CPNameAndTypeEntry name descriptor) = do
    nameIndex <- transformEntry (CPUTF8Entry name)
    descriptorIndex <- transformEntry (CPUTF8Entry descriptor)
    lookupOrInsertMCP (NameAndTypeInfo (fromIntegral nameIndex) (fromIntegral descriptorIndex))
transformEntry (CPFieldRefEntry (FieldRef classRef name fieldType)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (fieldTypeDescriptor fieldType))
    lookupOrInsertMCP (FieldRefInfo (fromIntegral classIndex) (fromIntegral nameAndTypeIndex))
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
    lookupOrInsertMCP
        ( InvokeDynamicInfo
            ( fromIntegral bmIndex - 1 -- bootstrap methods are 0 indexed because of course they are
            )
            (fromIntegral nameAndTypeIndex)
        )
transformEntry (CPMethodTypeEntry methodDescriptor) = do
    descriptorIndex <- transformEntry (CPUTF8Entry (convertMethodDescriptor methodDescriptor))
    lookupOrInsertMCP (MethodTypeInfo (fromIntegral descriptorIndex))

convertBootstrapMethod :: (Member ConstantPool r) => BootstrapMethod -> Sem r Int
convertBootstrapMethod (BootstrapMethod mhEntry args) = do
    mhIndex <- findIndexOf (CPMethodHandleEntry mhEntry)
    bsArgs <- traverse (findIndexOf . bmArgToCPEntry) args
    let bootstrapMethod = Raw.BootstrapMethod (fromIntegral mhIndex) (V.fromList bsArgs)
    lookupOrInsertMBM bootstrapMethod

instance Semigroup ConstantPoolState where
    (ConstantPoolState cp1 bm1) <> (ConstantPoolState cp2 bm2) = ConstantPoolState (cp1 <> cp2) (bm1 <> bm2)

instance Monoid ConstantPoolState where
    mempty = ConstantPoolState mempty mempty

findIndexOf :: (Member ConstantPool r) => ConstantPoolEntry -> Sem r U2
findIndexOf = fmap toU2OrError . transformEntry
  where
    toU2OrError :: Int -> U2
    toU2OrError i =
        if i > fromIntegral (maxBound @U2)
            then error "Constant pool index out of bounds, too many entries?"
            else fromIntegral i

constantPoolToState :: (Member (State ConstantPoolState) r) => Sem (ConstantPool ': r) a -> Sem r a
constantPoolToState = interpret $ \case
    GetCP -> gets id
    SetCP s -> modify (const s)

runConstantPoolWith :: ConstantPoolState -> Sem (ConstantPool ': r) a -> Sem r (a, ConstantPoolState)
runConstantPoolWith s =
    fmap swap
        . runState s
        . constantPoolToState
        . raiseUnder

runConstantPool :: Sem (ConstantPool ': r) a -> Sem r (a, ConstantPoolState)
runConstantPool = runConstantPoolWith mempty
