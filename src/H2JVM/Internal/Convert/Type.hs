{-# LANGUAGE LambdaCase #-}

module H2JVM.Internal.Convert.Type (fieldTypeDescriptor, classInfoTypeDescriptor, baseTypeDescriptor) where

import Data.Text (Text)

import H2JVM.Name (toInternalName)

import H2JVM.Type qualified as Abs

baseTypeDescriptor :: Abs.PrimitiveType -> Text
baseTypeDescriptor = \case
    Abs.Byte -> "B"
    Abs.Char -> "C"
    Abs.Double -> "D"
    Abs.Float -> "F"
    Abs.Int -> "I"
    Abs.Long -> "J"
    Abs.Short -> "S"
    Abs.Boolean -> "Z"

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
