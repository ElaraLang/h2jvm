{-# LANGUAGE LambdaCase #-}

module H2JVM.Internal.Convert.Type (fieldTypeDescriptor, classInfoTypeDescriptor, baseTypeDescriptor) where

import Data.Text (Text)

import H2JVM.Name (toInternalName)

import H2JVM.Type qualified as Abs

baseTypeDescriptor :: Abs.PrimitiveType -> Text
baseTypeDescriptor = \case
    Abs.JByte -> "B"
    Abs.JChar -> "C"
    Abs.JDouble -> "D"
    Abs.JFloat -> "F"
    Abs.JInt -> "I"
    Abs.JLong -> "J"
    Abs.JShort -> "S"
    Abs.JBoolean -> "Z"

fieldTypeDescriptor :: Abs.FieldType -> Text
fieldTypeDescriptor = \case
    Abs.PrimitiveFieldType base -> baseTypeDescriptor base
    Abs.ObjectFieldType name -> "L" <> toInternalName name <> ";"
    Abs.ArrayFieldType fieldType -> "[" <> fieldTypeDescriptor fieldType

classInfoTypeDescriptor :: Abs.ClassInfoType -> Text
classInfoTypeDescriptor = \case
    Abs.ClassInfoType name -> toInternalName name
    arrayType@(Abs.ArrayClassInfoType _) ->
        fieldTypeDescriptor (Abs.classInfoTypeToFieldType arrayType)
    Abs.PrimitiveClassInfoType base -> baseTypeDescriptor base
