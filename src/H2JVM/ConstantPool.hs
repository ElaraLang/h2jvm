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

-- | A reference to a field, consisting of the containing class type, field name, and field type.
data FieldRef = FieldRef ClassInfoType Text FieldType
    deriving (Data, Eq, Ord, Show)

instance Pretty FieldRef where
    pretty (FieldRef c n t) = pretty c <> "." <> pretty n <> ":" <> pretty t

-- | A reference to a method, consisting of the containing class type, method name, and descriptor signature.
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

-- | A Bootstrap Method specification, used in @invokedynamic@ call sites (§4.7.23).
data BootstrapMethod
    = -- | Construct a bootstrap method referencing a method handle and its static arguments.
      BootstrapMethod MethodHandleEntry [BootstrapArgument]
    deriving (Data, Eq, Ord, Show)

instance Pretty BootstrapMethod where
    pretty (BootstrapMethod mh args) = pretty mh <+> hsep (map pretty args)

-- | A static argument for a bootstrap method.
data BootstrapArgument
    = -- | A Class reference argument.
      BMClassArg ClassInfoType
    | -- | A String constant argument.
      BMStringArg Text
    | -- | An integer constant argument.
      BMIntArg Int32
    | -- | A Method type descriptor argument.
      BMMethodArg MethodDescriptor
    | -- | A Method handle argument.
      BMMethodHandleArg MethodHandleEntry
    deriving (Data, Eq, Ord, Show)

instance Pretty BootstrapArgument where
    pretty (BMClassArg c) = pretty c
    pretty (BMStringArg s) = pretty s
    pretty (BMIntArg i) = pretty i
    pretty (BMMethodArg m) = pretty m
    pretty (BMMethodHandleArg m) = pretty m

-- | Convert a bootstrap argument to its equivalent constant pool entry.
bmArgToCPEntry :: BootstrapArgument -> ConstantPoolEntry
bmArgToCPEntry (BMClassArg c) = CPClassEntry c
bmArgToCPEntry (BMStringArg s) = CPStringEntry s
bmArgToCPEntry (BMIntArg i) = CPIntegerEntry i
bmArgToCPEntry (BMMethodArg m) = CPMethodTypeEntry m
bmArgToCPEntry (BMMethodHandleArg m) = CPMethodHandleEntry m

-- | A Method Handle type entry in the constant pool (§4.4.8).
data MethodHandleEntry
    = -- | read field
      MHGetField FieldRef
    | -- | read static field
      MHGetStatic FieldRef
    | -- | write field
      MHPutField FieldRef
    | -- | write static field
      MHPutStatic FieldRef
    | -- | invoke virtual method
      MHInvokeVirtual MethodRef
    | -- | invoke special method (e.g. @<init>@)
      MHNewInvokeSpecial MethodRef
    | -- | invoke static method
      MHInvokeStatic MethodRef
    | -- | invoke private/super method
      MHInvokeSpecial MethodRef
    | -- | invoke interface method
      MHInvokeInterface MethodRef
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
