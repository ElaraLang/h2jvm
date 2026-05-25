{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Convert.Field (convertField) where

import Data.Word (Word16)
import Effectful
import Witch

import Data.Vector qualified as V

import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..))
import JVM.Data.Convert.AccessFlag (accessFlagsToWord16)
import JVM.Data.Convert.ConstantPool
import JVM.Data.Convert.Monad
import JVM.Data.Convert.Type (fieldTypeDescriptor)

import JVM.Data.Abstract.ClassFile.Field qualified as Abs
import JVM.Data.Raw.ClassFile qualified as Raw

convertConstantValue :: ConvertEff r => Abs.ConstantValue -> Eff r Word16
convertConstantValue =
    fmap into . findIndexOf . \case
        Abs.ConstantInteger i -> CPIntegerEntry (into i)
        Abs.ConstantFloat f -> CPFloatEntry f
        Abs.ConstantLong l -> CPLongEntry (into l)
        Abs.ConstantDouble d -> CPDoubleEntry d
        Abs.ConstantString s -> CPStringEntry s

convertFieldAttribute :: ConvertEff r => Abs.FieldAttribute -> Eff r Raw.AttributeInfo
convertFieldAttribute (Abs.ConstantValue constantValue) = do
    nameIndex <- findIndexOf (CPUTF8Entry "ConstantValue")
    constantValueIndex <- convertConstantValue constantValue
    pure $ Raw.AttributeInfo (into nameIndex) (Raw.ConstantValueAttribute constantValueIndex)
convertFieldAttribute _ = error "convertFieldAttribute"

convertField :: ConvertEff r => Abs.ClassFileField -> Eff r Raw.FieldInfo
convertField Abs.ClassFileField{..} = do
    nameIndex <- findIndexOf (CPUTF8Entry fieldName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (fieldTypeDescriptor fieldType))
    attributes <- traverse convertFieldAttribute fieldAttributes
    pure $ Raw.FieldInfo (accessFlagsToWord16 fieldAccessFlags) (into nameIndex) (into descriptorIndex) (V.fromList attributes)
