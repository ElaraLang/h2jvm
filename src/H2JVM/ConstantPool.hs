{-# LANGUAGE PartialTypeSignatures #-}

{- | Provides a monadic interface to the constant pool.
This aims to eliminate the need to manually specify the index of the constant
-}
module H2JVM.ConstantPool (ConstantPoolEntry (..), MethodHandleEntry (..), FieldRef (..), MethodRef (..), BootstrapMethod (..), BootstrapArgument (..), bmArgToCPEntry) where

import Data.Data
import Data.Int (Int32, Int64)
import Data.Text (Text)

import H2JVM.Descriptor (MethodDescriptor)
import H2JVM.Internal.Pretty
import H2JVM.Type (ClassInfoType, FieldType)

{- | High-level, type-safe representation of a constant pool entry
This tries to hide indexes as much as possible, instead just allowing the values to be provided directly.
These are transformed into the correct indexes when the constant pool is built, which uses a state monad to avoid repeating entries.
-}
data ConstantPoolEntry
    = -- | A class reference
      CPClassEntry
        -- | The class being referenced
        ClassInfoType
    | CPFieldRefEntry FieldRef
    | CPMethodRefEntry MethodRef
    | CPInterfaceMethodRefEntry MethodRef
    | CPStringEntry Text
    | CPIntegerEntry Int32
    | CPFloatEntry Float
    | CPLongEntry Int64
    | CPDoubleEntry Double
    | CPNameAndTypeEntry Text Text
    | CPUTF8Entry Text
    | CPMethodHandleEntry MethodHandleEntry
    | CPMethodTypeEntry MethodDescriptor
    | -- | CONSTANT_InvokeDynamic_info
      CPInvokeDynamicEntry
        -- | bootstrap_method_attr(_index)
        BootstrapMethod
        -- | name(_and_type_index)
        Text
        -- | (name_and_)type(_index)
        MethodDescriptor
    deriving (Eq, Ord, Show)

data FieldRef = FieldRef ClassInfoType Text FieldType
    deriving (Data, Eq, Ord, Show)

instance Pretty FieldRef where
    pretty (FieldRef c n t) = pretty c <> "." <> pretty n <> ":" <> pretty t

data MethodRef
    = MethodRef
        -- | The class containing the method
        ClassInfoType
        -- | The name of the method
        Text
        -- | The descriptor of the method
        MethodDescriptor
    deriving (Data, Eq, Ord, Show)

instance Pretty MethodRef where
    pretty (MethodRef c n d) = pretty c <> "." <> pretty n <> pretty d

-- | A JVM Bootstrap Method.
data BootstrapMethod
    = BootstrapMethod MethodHandleEntry [BootstrapArgument]
    deriving (Data, Eq, Ord, Show)

instance Pretty BootstrapMethod where
    pretty (BootstrapMethod mh args) = pretty mh <+> hsep (map pretty args)

data BootstrapArgument
    = BMClassArg ClassInfoType
    | BMStringArg Text
    | BMIntArg Int32
    | BMMethodArg MethodDescriptor
    | BMMethodHandleArg MethodHandleEntry
    deriving (Data, Eq, Ord, Show)

instance Pretty BootstrapArgument where
    pretty (BMClassArg c) = pretty c
    pretty (BMStringArg s) = pretty s
    pretty (BMIntArg i) = pretty i
    pretty (BMMethodArg m) = pretty m
    pretty (BMMethodHandleArg m) = pretty m

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
    deriving (Data, Eq, Ord, Show)

instance Pretty MethodHandleEntry where
    pretty (MHGetField f) = "getField" <+> pretty f
    pretty (MHGetStatic f) = "getStatic" <+> pretty f
    pretty (MHPutField f) = "putField" <+> pretty f
    pretty (MHPutStatic f) = "putStatic" <+> pretty f
    pretty (MHInvokeVirtual m) = "invokeVirtual" <+> pretty m
    pretty (MHNewInvokeSpecial m) = "newInvokeSpecial" <+> pretty m
    pretty (MHInvokeStatic m) = "invokeStatic" <+> pretty m
    pretty (MHInvokeSpecial m) = "invokeSpecial" <+> pretty m
    pretty (MHInvokeInterface m) = "invokeInterface" <+> pretty m
