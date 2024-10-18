module JVM.Data.Abstract.ClassFile.Field where

import Data.Text (Text)
import JVM.Data.Abstract.ClassFile.AccessFlags (FieldAccessFlag)
import JVM.Data.Abstract.Type (FieldType)
import JVM.Data.Pretty (Pretty (pretty))
import JVM.Data.Raw.Types (JVMDouble, JVMFloat, JVMInt, JVMLong, JVMString)

data ClassFileField = ClassFileField
    { fieldAccessFlags :: [FieldAccessFlag]
    , fieldName :: Text
    , fieldType :: FieldType
    , fieldAttributes :: [FieldAttribute]
    }
    deriving (Show)

data FieldAttribute
    = ConstantValue ConstantValue
    | Synthetic
    deriving (Show)

data ConstantValue
    = ConstantLong JVMLong
    | ConstantFloat JVMFloat
    | ConstantDouble JVMDouble
    | ConstantInteger JVMInt
    | ConstantString JVMString
    deriving (Show)

instance Pretty ClassFileField where
    pretty (ClassFileField accessFlags name fieldType attributes) =
        pretty accessFlags <> " " <> pretty fieldType <> " " <> pretty name <> " " <> pretty attributes

instance Pretty FieldAttribute where
    pretty (ConstantValue v) = "ConstantValue " <> pretty v
    pretty Synthetic = "Synthetic"

instance Pretty ConstantValue where
    pretty (ConstantLong v) = "ConstantLong " <> pretty v
    pretty (ConstantFloat v) = "ConstantFloat " <> pretty v
    pretty (ConstantDouble v) = "ConstantDouble " <> pretty v
    pretty (ConstantInteger v) = "ConstantInteger " <> pretty v
    pretty (ConstantString v) = "ConstantString " <> pretty v