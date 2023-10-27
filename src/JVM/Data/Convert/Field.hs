{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Convert.Field where

import Data.Vector qualified as V
import Data.Word (Word16)
import JVM.Data.Abstract.ClassFile.Field qualified as Abs
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (..))
import JVM.Data.Convert.AccessFlag (accessFlagsToWord16)
import JVM.Data.Convert.ConstantPool
import JVM.Data.Convert.Monad (ConvertM)
import JVM.Data.Convert.Type (fieldTypeDescriptor)
import JVM.Data.Raw.ClassFile qualified as Raw

convertConstantValue :: Abs.ConstantValue -> ConvertM Word16
convertConstantValue =
    fmap fromIntegral . findIndexOf . \case
        Abs.ConstantInteger i -> CPIntegerEntry (fromIntegral i)
        Abs.ConstantFloat f -> CPFloatEntry f
        Abs.ConstantLong l -> CPLongEntry (fromIntegral l)
        Abs.ConstantDouble d -> CPDoubleEntry d
        Abs.ConstantString s -> CPStringEntry s

convertFieldAttribute :: Abs.FieldAttribute -> ConvertM Raw.AttributeInfo
convertFieldAttribute (Abs.ConstantValue constantValue) = do
    nameIndex <- findIndexOf (CPUTF8Entry "ConstantValue")
    constantValueIndex <- convertConstantValue constantValue
    pure $ Raw.AttributeInfo (fromIntegral nameIndex) (Raw.ConstantValueAttribute constantValueIndex)
convertFieldAttribute _ = error "convertFieldAttribute"

convertField :: Abs.ClassFileField -> ConvertM Raw.FieldInfo
convertField Abs.ClassFileField{..} = do
    nameIndex <- findIndexOf (CPUTF8Entry fieldName)
    descriptorIndex <- findIndexOf (CPUTF8Entry (fieldTypeDescriptor fieldType))
    attributes <- traverse convertFieldAttribute fieldAttributes
    pure $ Raw.FieldInfo (accessFlagsToWord16 fieldAccessFlags) (fromIntegral nameIndex) (fromIntegral descriptorIndex) (V.fromList attributes)
