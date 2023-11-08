module JVM.Data.Abstract.Type where

import Data.Data
import JVM.Data.Abstract.Name (QualifiedClassName)
import JVM.Data.Pretty (Pretty (pretty))

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

instance Pretty PrimitiveType where
    pretty Byte = "byte"
    pretty Char = "char"
    pretty Double = "double"
    pretty Float = "float"
    pretty Int = "int"
    pretty Long = "long"
    pretty Short = "short"
    pretty Boolean = "boolean"

data FieldType
    = PrimitiveFieldType PrimitiveType
    | ObjectFieldType QualifiedClassName
    | ArrayFieldType FieldType
    deriving (Show, Eq, Ord, Data)

instance Pretty FieldType where
    pretty (PrimitiveFieldType p) = pretty p
    pretty (ObjectFieldType c) = pretty c
    pretty (ArrayFieldType f) = pretty f <> "[]"

fieldTypeToClassInfoType :: FieldType -> ClassInfoType
fieldTypeToClassInfoType (PrimitiveFieldType p) = PrimitiveClassInfoType p
fieldTypeToClassInfoType (ObjectFieldType c) = ClassInfoType c
fieldTypeToClassInfoType (ArrayFieldType f) = ArrayClassInfoType (fieldTypeToClassInfoType f)

data ClassInfoType
    = ClassInfoType QualifiedClassName
    | PrimitiveClassInfoType PrimitiveType
    | ArrayClassInfoType ClassInfoType
    deriving (Show, Eq, Ord, Data)

instance Pretty ClassInfoType where
    pretty (ClassInfoType c) = pretty c
    pretty (PrimitiveClassInfoType p) = pretty p
    pretty (ArrayClassInfoType c) = pretty c <> "[]"
