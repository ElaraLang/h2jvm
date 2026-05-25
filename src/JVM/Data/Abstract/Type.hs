module JVM.Data.Abstract.Type (
    ClassInfoType (..),
    PrimitiveType (..),
    FieldType (..),
    fieldTypeToClassInfoType,
    classInfoTypeToFieldType,
)
where

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
    deriving (Bounded, Data, Enum, Eq, Ord, Show)

instance Pretty PrimitiveType where
    pretty Byte = "byte"
    pretty Char = "char"
    pretty Double = "double"
    pretty Float = "float"
    pretty Int = "int"
    pretty Long = "long"
    pretty Short = "short"
    pretty Boolean = "boolean"

{- | JVM field type.
Used in places such as method descriptors and field descriptors.
-}
data FieldType
    = PrimitiveFieldType PrimitiveType
    | ObjectFieldType QualifiedClassName
    | ArrayFieldType FieldType
    deriving (Data, Eq, Ord, Show)

instance Pretty FieldType where
    pretty (PrimitiveFieldType p) = pretty p
    pretty (ObjectFieldType c) = pretty c
    pretty (ArrayFieldType f) = pretty f <> "[]"

fieldTypeToClassInfoType :: FieldType -> ClassInfoType
fieldTypeToClassInfoType (PrimitiveFieldType p) = PrimitiveClassInfoType p
fieldTypeToClassInfoType (ObjectFieldType c) = ClassInfoType c
fieldTypeToClassInfoType (ArrayFieldType f) = ArrayClassInfoType (fieldTypeToClassInfoType f)

{- | JVM class info type.
Used in places such as the constant pool and exception tables.
-}
data ClassInfoType
    = ClassInfoType QualifiedClassName
    | PrimitiveClassInfoType PrimitiveType
    | ArrayClassInfoType ClassInfoType
    deriving (Data, Eq, Ord, Show)

classInfoTypeToFieldType :: ClassInfoType -> FieldType
classInfoTypeToFieldType (ClassInfoType c) = ObjectFieldType c
classInfoTypeToFieldType (PrimitiveClassInfoType p) = PrimitiveFieldType p
classInfoTypeToFieldType (ArrayClassInfoType c) = ArrayFieldType (classInfoTypeToFieldType c)

instance Pretty ClassInfoType where
    pretty (ClassInfoType c) = pretty c
    pretty (PrimitiveClassInfoType p) = pretty p
    pretty (ArrayClassInfoType c) = pretty c <> "[]"
