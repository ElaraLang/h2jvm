module JVM.Data.Abstract.Field where

import Data.Text (Text)
import JVM.Data.Abstract.AccessFlags (FieldAccessFlag)
import JVM.Data.Abstract.Type (FieldType)
import JVM.Data.Raw.Types (JVMDouble, JVMFloat, JVMInt, JVMLong, JVMString)

data ClassFileField = ClassFileField
    { fieldAccessFlags :: [FieldAccessFlag]
    , fieldName :: Text
    , fieldType :: FieldType
    , fieldAttributes :: [FieldAttribute]
    }

data FieldAttribute
    = ConstantValue ConstantValue
    | Synthetic

data ConstantValue
    = ConstantLong JVMLong
    | ConstantFloat JVMFloat
    | ConstantDouble JVMDouble
    | ConstantInteger JVMInt
    | ConstantString JVMString
