{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Conversion of fields to the raw, low level format.
module H2JVM.Internal.Convert.Field (convertField) where

import Data.Word (Word16)
import Effectful
import Witch

import Data.Vector qualified as V

import H2JVM.ConstantPool (ConstantPoolEntry (..))
import H2JVM.Internal.Convert.AccessFlag (accessFlagsToWord16)
import H2JVM.Internal.Convert.ConstantPool
import H2JVM.Internal.Convert.Monad
import H2JVM.Internal.Convert.Type (fieldTypeDescriptor)

import H2JVM.ClassFile.Field qualified as Abs
import H2JVM.Internal.Raw.ClassFile qualified as Raw

-- | Convert a 'Abs.ConstantValue' to an index within the constant pool.
convertConstantValue :: ConvertEff r => Abs.ConstantValue -> Eff r Word16
convertConstantValue =
    fmap into . findIndexOf . \case
        Abs.ConstantInteger i -> CPIntegerEntry (into i)
        Abs.ConstantFloat f -> CPFloatEntry f
        Abs.ConstantLong l -> CPLongEntry (into l)
        Abs.ConstantDouble d -> CPDoubleEntry d
        Abs.ConstantString s -> CPStringEntry s

-- | Convert a 'Abs.FieldAttribute' to a raw attribute info by upserting its components into the constant pool.
convertFieldAttribute :: ConvertEff r => Abs.FieldAttribute -> Eff r Raw.AttributeInfo
convertFieldAttribute (Abs.ConstantValue constantValue) = do
    nameIndex <- findIndexOf (CPUTF8Entry "ConstantValue")
    constantValueIndex <- convertConstantValue constantValue
    pure $ Raw.AttributeInfo (into nameIndex) (Raw.ConstantValueAttribute constantValueIndex)
convertFieldAttribute Abs.Synthetic = do
    nameIndex <- findIndexOf (CPUTF8Entry "Synthetic")
    pure $ Raw.AttributeInfo (into nameIndex) Raw.SyntheticAttribute

-- | Convert a 'Abs.ClassFileField' to a 'Raw.FieldInfo' by upserting all its attributes into the constant poo..
convertField :: ConvertEff r => Abs.ClassFileField -> Eff r Raw.FieldInfo
convertField Abs.ClassFileField{..} = do
    nameIndex <- findIndexOf (CPUTF8Entry fieldName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (fieldTypeDescriptor fieldType))
    attributes <- traverse convertFieldAttribute fieldAttributes
    pure $ Raw.FieldInfo (accessFlagsToWord16 fieldAccessFlags) (into nameIndex) (into descriptorIndex) (V.fromList attributes)
