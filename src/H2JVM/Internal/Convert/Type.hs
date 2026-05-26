{-# LANGUAGE LambdaCase #-}

-- | Conversions of high level descriptor types to their low level textual representations.
module H2JVM.Internal.Convert.Type (
    fieldTypeDescriptor,
    classInfoTypeDescriptor,
    baseTypeDescriptor,
    convertMethodDescriptor,
)
where

import Data.Text (Text)

import H2JVM.Descriptor
import H2JVM.Name (toInternalName)

import H2JVM.Type qualified as Abs

-- | Get the internal descriptor text of a 'Abs.PrimitiveType'.
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

-- | Get the internal textual descriptor of a 'Abs.FieldType'.
fieldTypeDescriptor :: Abs.FieldType -> Text
fieldTypeDescriptor = \case
    Abs.PrimitiveFieldType base -> baseTypeDescriptor base
    Abs.ObjectFieldType name -> "L" <> toInternalName name <> ";"
    Abs.ArrayFieldType fieldType -> "[" <> fieldTypeDescriptor fieldType

-- | Get the internal textual descriptor of a 'Abs.ClassInfoType'.
classInfoTypeDescriptor :: Abs.ClassInfoType -> Text
classInfoTypeDescriptor = \case
    Abs.ClassInfoType name -> toInternalName name
    arrayType@(Abs.ArrayClassInfoType _) ->
        fieldTypeDescriptor (Abs.classInfoTypeToFieldType arrayType)
    Abs.PrimitiveClassInfoType base -> baseTypeDescriptor base

-- | Convert a 'MethodDescriptor' to its low level textual representation.
convertMethodDescriptor :: MethodDescriptor -> Text
convertMethodDescriptor (MethodDescriptor params ret) =
    let params' = map fieldTypeDescriptor params
        ret' = case ret of
            VoidReturn -> "V"
            TypeReturn t -> fieldTypeDescriptor t
     in "(" <> mconcat params' <> ")" <> ret'
