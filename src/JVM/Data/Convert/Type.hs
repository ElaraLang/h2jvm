{-# LANGUAGE LambdaCase #-}

module JVM.Data.Convert.Type where

import Data.Text (Text)
import JVM.Data.Abstract.Name (toInternalName)
import JVM.Data.Abstract.Type qualified as Abs

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
    Abs.ArrayClassInfoType classInfoType -> "[" <> classInfoTypeDescriptor classInfoType
    Abs.PrimitiveClassInfoType base -> baseTypeDescriptor base
