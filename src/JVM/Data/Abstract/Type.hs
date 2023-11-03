module JVM.Data.Abstract.Type where

import JVM.Data.Abstract.Name (QualifiedClassName)
import Data.Data

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

data ClassInfoType
    = ClassInfoType QualifiedClassName
    | ArrayClassInfoType ClassInfoType
    deriving (Show, Eq, Ord, Data)
