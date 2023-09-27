{-# LANGUAGE PartialTypeSignatures #-}

{- | Provides a monadic interface to the constant pool.
 This aims to eliminate the need to manually specify the index of the constant
-}
module JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..), FieldRef (..), MethodRef (..), findIndexOf, runConstantPoolM, ConstantPoolT, ConstantPoolM, runConstantPoolT) where

import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.IndexedMap (IndexedMap)
import Data.IndexedMap qualified as IM
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import JVM.Data.Abstract.Descriptor (BootstrapMethodDescriptor, MethodDescriptor)
import JVM.Data.Abstract.Type (ClassInfoType, FieldType)
import JVM.Data.Convert.Descriptor (convertMethodDescriptor)
import JVM.Data.Convert.Numbers (toJVMFloat, toJVMLong)
import JVM.Data.Convert.Type (classInfoTypeDescriptor, fieldTypeDescriptor)
import JVM.Data.Raw.ConstantPool (ConstantPoolInfo (..))
import JVM.Data.Raw.Types (U2)

{- | High-level, type-safe representation of a constant pool entry
 This tries to hide indexes as much as possible, instead just allowing the values to be provided directly.
 These are transformed into the correct indexes when the constant pool is built, which uses a state monad to avoid repeating entries.
-}
data ConstantPoolEntry
    = -- | A class reference
      CPClassEntry ClassInfoType
                   -- ^ The class being referenced
    | CPFieldRefEntry FieldRef
    | CPMethodRefEntry MethodRef
    | CPInterfaceMethodRefEntry Text ()
    | CPStringEntry Text
    | CPIntegerEntry Int
    | CPFloatEntry Float
    | CPLongEntry Int64
    | CPDoubleEntry Double
    | CPNameAndTypeEntry Text Text
    | CPUTF8Entry Text
    | CPMethodHandleEntry MethodHandleEntry
    | CPMethodTypeEntry Text -- TODO and this one
    | -- | CONSTANT_InvokeDynamic_info
      CPInvokeDynamicEntry
        BootstrapMethodDescriptor
        -- ^ bootstrap_method_attr(_index)
        Text
        -- ^ name(_and_type_index)
        MethodDescriptor
        -- ^ (name_and_)type(_index)
    deriving (Show, Eq, Ord)

data FieldRef = FieldRef ClassInfoType Text FieldType
    deriving (Show, Eq, Ord)

data MethodRef
    = MethodRef
        ClassInfoType
        -- ^ The class containing the method
        Text
        -- ^ The name of the method
        MethodDescriptor
        -- ^ The descriptor of the method
    deriving (Show, Eq, Ord)

data MethodHandleEntry
    = MHGetField FieldRef
    | MHGetStatic FieldRef
    | MHPutField FieldRef
    | MHPutStatic FieldRef
    | MHInvokeVirtual MethodRef
    | MHNewInvokeSpecial MethodRef
    | MHInvokeStatic MethodRef
    | MHInvokeSpecial MethodRef
    | MHInvokeInterface MethodRef
    deriving (Show, Eq, Ord)

transformEntry :: ConstantPoolEntry -> ConstantPoolM Int
transformEntry (CPUTF8Entry text) = IM.lookupOrInsertM (UTF8Info $ encodeUtf8 text)
transformEntry (CPIntegerEntry i) = IM.lookupOrInsertM (IntegerInfo $ fromIntegral i)
transformEntry (CPFloatEntry f) = IM.lookupOrInsertM (FloatInfo (toJVMFloat f))
transformEntry (CPStringEntry msg) = do
    i <- transformEntry (CPUTF8Entry msg)
    IM.lookupOrInsertM (StringInfo $ fromIntegral i)
transformEntry (CPLongEntry i) = do
    let (high, low) = toJVMLong i
    IM.lookupOrInsertM (LongInfo high low)
transformEntry (CPDoubleEntry d) = do
    let (high, low) = toJVMLong (round d)
    IM.lookupOrInsertM (DoubleInfo high low)
transformEntry (CPClassEntry name) = do
    let className = classInfoTypeDescriptor name
    nameIndex <- transformEntry (CPUTF8Entry className)
    IM.lookupOrInsertM (ClassInfo $ fromIntegral nameIndex)
transformEntry (CPMethodRefEntry (MethodRef classRef name methodDescriptor)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (convertMethodDescriptor methodDescriptor))
    IM.lookupOrInsertM (MethodRefInfo (fromIntegral classIndex) (fromIntegral nameAndTypeIndex))
transformEntry (CPNameAndTypeEntry name descriptor) = do
    nameIndex <- transformEntry (CPUTF8Entry name)
    descriptorIndex <- transformEntry (CPUTF8Entry descriptor)
    IM.lookupOrInsertM (NameAndTypeInfo (fromIntegral nameIndex) (fromIntegral descriptorIndex))
transformEntry (CPFieldRefEntry (FieldRef classRef name fieldType)) = do
    classIndex <- transformEntry (CPClassEntry classRef)
    nameAndTypeIndex <- transformEntry (CPNameAndTypeEntry name (fieldTypeDescriptor fieldType))
    IM.lookupOrInsertM (FieldRefInfo (fromIntegral classIndex) (fromIntegral nameAndTypeIndex))

type ConstantPoolT m a = StateT (IndexedMap ConstantPoolInfo) m a

type ConstantPoolM a = ConstantPoolT Identity a

findIndexOf :: ConstantPoolEntry -> ConstantPoolM U2
findIndexOf = fmap fromIntegral . transformEntry

runConstantPoolM :: ConstantPoolM a -> (a, Vector ConstantPoolInfo)
runConstantPoolM m = let (a, cp) = runState m IM.empty in (a, IM.toVector cp)

runConstantPoolT :: Monad m => ConstantPoolT m a2 -> m (a2, Vector ConstantPoolInfo)
runConstantPoolT m = do
    (a, cp) <- runStateT m IM.empty
    pure (a, IM.toVector cp)
