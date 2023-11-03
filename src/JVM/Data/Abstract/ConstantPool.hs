{-# LANGUAGE PartialTypeSignatures #-}

{- | Provides a monadic interface to the constant pool.
 This aims to eliminate the need to manually specify the index of the constant
-}
module JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..), MethodHandleEntry (..), FieldRef (..), MethodRef (..), BootstrapMethod (..), BootstrapArgument (..), bmArgToCPEntry) where

import Data.Int (Int64)
import Data.Text (Text)
import JVM.Data.Abstract.Descriptor (MethodDescriptor)

import Data.Data
import JVM.Data.Abstract.Type (ClassInfoType, FieldType)

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
    | CPInterfaceMethodRefEntry MethodRef
    | CPStringEntry Text
    | CPIntegerEntry Int
    | CPFloatEntry Float
    | CPLongEntry Int64
    | CPDoubleEntry Double
    | CPNameAndTypeEntry Text Text
    | CPUTF8Entry Text
    | CPMethodHandleEntry MethodHandleEntry
    | CPMethodTypeEntry MethodDescriptor
    | -- | CONSTANT_InvokeDynamic_info
      CPInvokeDynamicEntry
        BootstrapMethod
        -- ^ bootstrap_method_attr(_index)
        Text
        -- ^ name(_and_type_index)
        MethodDescriptor
        -- ^ (name_and_)type(_index)
    deriving (Show, Eq, Ord)

data FieldRef = FieldRef ClassInfoType Text FieldType
    deriving (Show, Eq, Ord, Data)

data MethodRef
    = MethodRef
        ClassInfoType
        -- ^ The class containing the method
        Text
        -- ^ The name of the method
        MethodDescriptor
        -- ^ The descriptor of the method
    deriving (Show, Eq, Ord, Data)

data BootstrapMethod
    = BootstrapMethod MethodHandleEntry [BootstrapArgument]
    deriving (Show, Eq, Ord, Data)

data BootstrapArgument
    = BMClassArg ClassInfoType
    | BMStringArg Text
    | BMIntArg Int
    | BMMethodArg MethodDescriptor
    | BMMethodHandleArg MethodHandleEntry
    deriving (Show, Eq, Ord, Data)

bmArgToCPEntry :: BootstrapArgument -> ConstantPoolEntry
bmArgToCPEntry (BMClassArg c) = CPClassEntry c
bmArgToCPEntry (BMStringArg s) = CPStringEntry s
bmArgToCPEntry (BMIntArg i) = CPIntegerEntry i
bmArgToCPEntry (BMMethodArg m) = CPMethodTypeEntry m
bmArgToCPEntry (BMMethodHandleArg m) = CPMethodHandleEntry m

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
    deriving (Show, Eq, Ord, Data)
