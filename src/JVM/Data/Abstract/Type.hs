module JVM.Data.Abstract.Type where

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
    deriving (Show, Eq, Ord)

data FieldType
    = PrimitiveFieldType PrimitiveType
    | ObjectFieldType QualifiedClassName
    | ArrayFieldType FieldType
    deriving (Show, Eq, Ord)

data ClassInfoType
    = ClassInfoType QualifiedClassName
    | ArrayClassInfoType ClassInfoType
    deriving (Show, Eq, Ord)
