module JVM.Data.Abstract.Type where

import Data.Data
import JVM.Data.Abstract.Name (QualifiedClassName)

data PrimitiveType
    = Byte
    | Char
    | Double
    | Float
    | Int
    | Long
    | Short
    | Boolean
    deriving (Show, Eq, Ord, Data)

data FieldType
    = PrimitiveFieldType PrimitiveType
    | ObjectFieldType QualifiedClassName
    | ArrayFieldType FieldType
    deriving (Show, Eq, Ord, Data)


fieldTypeToClassInfoType :: FieldType -> ClassInfoType
fieldTypeToClassInfoType (PrimitiveFieldType p) = PrimitiveClassInfoType p
fieldTypeToClassInfoType (ObjectFieldType c) = ClassInfoType c
fieldTypeToClassInfoType (ArrayFieldType f) = ArrayClassInfoType (fieldTypeToClassInfoType f)

data ClassInfoType
    = ClassInfoType QualifiedClassName
    | PrimitiveClassInfoType PrimitiveType
    | ArrayClassInfoType ClassInfoType
    deriving (Show, Eq, Ord, Data)
