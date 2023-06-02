{-# LANGUAGE LambdaCase #-}

module JVM.Data.Convert.Type where

import Data.Text (Text)
import JVM.Data.Abstract.Name (toInternalName)
import JVM.Data.Abstract.Type qualified as Abs

baseTypeDescriptor :: Abs.BaseType -> Text
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
    Abs.BaseFieldType base -> baseTypeDescriptor base
    Abs.ObjectFieldType name -> "L" <> toInternalName name <> ";"
    Abs.ArrayFieldType fieldType -> "[" <> fieldTypeDescriptor fieldType
