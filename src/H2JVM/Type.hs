-- | Types representing JVM types, such as field types and class info types.
module H2JVM.Type (
    ClassInfoType (..),
    PrimitiveType (..),
    FieldType (..),
    fieldTypeToClassInfoType,
    classInfoTypeToFieldType,
)
where

import Data.Data

import H2JVM.Internal.Pretty (Pretty (pretty))
import H2JVM.Name (QualifiedClassName)

{- | JVM primitive types.
Corresponds to the @BaseType@ production in the JVM specification.
-}
data PrimitiveType
    = JByte
    | JChar
    | JDouble
    | JFloat
    | JInt
    | JLong
    | JShort
    | JBoolean
    deriving (Bounded, Data, Enum, Eq, Ord, Show)

instance Pretty PrimitiveType where
    pretty JByte = "byte"
    pretty JChar = "char"
    pretty JDouble = "double"
    pretty JFloat = "float"
    pretty JInt = "int"
    pretty JLong = "long"
    pretty JShort = "short"
    pretty JBoolean = "boolean"

{- | JVM field type, corresponding to the @FieldType@ production in the JVM specification.
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

-- | Convert a 'FieldType' to a 'ClassInfoType'
fieldTypeToClassInfoType :: FieldType -> ClassInfoType
fieldTypeToClassInfoType (PrimitiveFieldType p) = PrimitiveClassInfoType p
fieldTypeToClassInfoType (ObjectFieldType c) = ClassInfoType c
fieldTypeToClassInfoType (ArrayFieldType f) = ArrayClassInfoType (fieldTypeToClassInfoType f)

{- | JVM class info type. Corresponds roughly to §4.2.1 of the JVM specification.
Used in places such as the constant pool and exception tables.
Isomorphic to 'FieldType', but used in different contexts and kept separate for clarity.
-}
data ClassInfoType
    = ClassInfoType QualifiedClassName
    | PrimitiveClassInfoType PrimitiveType
    | ArrayClassInfoType ClassInfoType
    deriving (Data, Eq, Ord, Show)

-- | Convert a 'ClassInfoType' to a 'FieldType'.
classInfoTypeToFieldType :: ClassInfoType -> FieldType
classInfoTypeToFieldType (ClassInfoType c) = ObjectFieldType c
classInfoTypeToFieldType (PrimitiveClassInfoType p) = PrimitiveFieldType p
classInfoTypeToFieldType (ArrayClassInfoType c) = ArrayFieldType (classInfoTypeToFieldType c)

instance Pretty ClassInfoType where
    pretty (ClassInfoType c) = pretty c
    pretty (PrimitiveClassInfoType p) = pretty p
    pretty (ArrayClassInfoType c) = pretty c <> "[]"
