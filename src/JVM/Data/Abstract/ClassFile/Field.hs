module JVM.Data.Abstract.ClassFile.Field where

import Data.Text (Text)
import JVM.Data.Abstract.ClassFile.AccessFlags (FieldAccessFlag)
import JVM.Data.Abstract.Type (FieldType)
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
