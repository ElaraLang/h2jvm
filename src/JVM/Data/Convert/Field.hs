{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Convert.Field where

import Data.Bits ((.|.))
import Data.Vector qualified as V
import Data.Word (Word16)
import JVM.Data.Abstract.AccessFlags qualified as Abs
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..), ConstantPoolM, findIndexOf)
import JVM.Data.Abstract.Field qualified as Abs
import JVM.Data.Convert.Type (fieldTypeDescriptor)
import JVM.Data.Raw.AccessFlags (AccessFlags (..), accessFlagValue)
import JVM.Data.Raw.ClassFile qualified as Raw

convertAccessFlags :: [Abs.FieldAccessFlag] -> Word16
convertAccessFlags = foldr ((.|.) . convertAccessFlag) 0
  where
    convertAccessFlag =
        accessFlagValue . \case
            Abs.FPublic -> ACC_PUBLIC
            Abs.FPrivate -> ACC_PRIVATE
            Abs.FProtected -> ACC_PROTECTED
            Abs.FStatic -> ACC_STATIC
            Abs.FFinal -> ACC_FINAL
            Abs.FVolatile -> ACC_VOLATILE
            Abs.FTransient -> ACC_TRANSIENT
            Abs.FSynthetic -> ACC_SYNTHETIC
            Abs.FEnum -> ACC_ENUM

convertConstantValue :: Abs.ConstantValue -> ConstantPoolM Word16
convertConstantValue =
    findIndexOf . \case
        Abs.ConstantInteger i -> CPIntegerEntry (fromIntegral i)
        Abs.ConstantFloat f -> CPFloatEntry f
        Abs.ConstantLong l -> CPLongEntry (fromIntegral l)
        Abs.ConstantDouble d -> CPDoubleEntry d
        Abs.ConstantString s -> CPStringEntry s

convertFieldAttribute :: Abs.FieldAttribute -> ConstantPoolM Raw.AttributeInfo
convertFieldAttribute (Abs.ConstantValue constantValue) = do
    nameIndex <- findIndexOf (CPUTF8Entry "ConstantValue")
    constantValueIndex <- convertConstantValue constantValue
    pure $ Raw.AttributeInfo nameIndex (Raw.ConstantValueAttribute constantValueIndex)
convertFieldAttribute _ = error "convertFieldAttribute"

convertField :: Abs.ClassFileField -> ConstantPoolM Raw.FieldInfo
convertField Abs.ClassFileField{..} = do
    nameIndex <- findIndexOf (CPUTF8Entry fieldName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (fieldTypeDescriptor fieldType))
    attributes <- traverse convertFieldAttribute fieldAttributes
    pure $ Raw.FieldInfo (convertAccessFlags fieldAccessFlags) nameIndex descriptorIndex (V.fromList attributes)
