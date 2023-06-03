module JVM.Data.Abstract.Type where

import JVM.Data.Abstract.Name (QualifiedClassName)

data BaseType
    = Byte
    | Char
    | Double
    | Float
    | Int
    | Long
    | Short
    | Boolean
    deriving (Show)

data FieldType
    = BaseFieldType BaseType
    | ObjectFieldType QualifiedClassName
    | ArrayFieldType FieldType
    deriving (Show)
