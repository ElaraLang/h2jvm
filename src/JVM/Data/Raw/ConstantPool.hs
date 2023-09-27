{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Raw.ConstantPool where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Write (WriteBinary (writeBinary))
import Data.ByteString
import Data.ByteString qualified as BS
import JVM.Data.Raw.MagicNumbers qualified as MagicNumbers
import JVM.Data.Raw.Types

data ConstantPoolInfo
    = -- | https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.1
      ClassInfo {nameIndex :: U2}
    | FieldRefInfo {classIndex :: U2, nameAndTypeIndex :: U2}
    | MethodRefInfo {classIndex :: U2, nameAndTypeIndex :: U2}
    | InterfaceMethodRefInfo {classIndex :: U2, nameAndTypeIndex :: U2}
    | StringInfo {stringIndex :: U2}
    | IntegerInfo {bytes :: U4}
    | FloatInfo {bytes :: U4}
    | LongInfo {highBytes :: U4, lowBytes :: U4}
    | DoubleInfo {highBytes :: U4, lowBytes :: U4}
    | NameAndTypeInfo {nameIndex :: U2, descriptorIndex :: U2}
    | UTF8Info {utfBytes :: ByteString}
    | MethodHandleInfo {referenceKind :: U1, referenceIndex :: U2}
    | MethodTypeInfo {descriptorIndex :: U2}
    | InvokeDynamicInfo {bootstrapMethodAttrIndex :: U2, nameAndTypeIndex :: U2}
    deriving (Show, Eq, Ord)

instance WriteBinary ConstantPoolInfo where
    writeBinary (ClassInfo{..}) = do
        putWord8 MagicNumbers.constant_Class
        putWord16be nameIndex
    writeBinary (FieldRefInfo{..}) = do
        putWord8 MagicNumbers.constant_Fieldref
        putWord16be classIndex
        putWord16be nameAndTypeIndex
    writeBinary (MethodRefInfo{..}) = do
        putWord8 MagicNumbers.constant_Methodref
        putWord16be classIndex
        putWord16be nameAndTypeIndex
    writeBinary (InterfaceMethodRefInfo{..}) = do
        putWord8 MagicNumbers.constant_InterfaceMethodref
        putWord16be classIndex
        putWord16be nameAndTypeIndex
    writeBinary (StringInfo{..}) = do
        putWord8 MagicNumbers.constant_String
        putWord16be stringIndex
    writeBinary (IntegerInfo{..}) = do
        putWord8 MagicNumbers.constant_Integer
        putWord32be bytes
    writeBinary (FloatInfo{..}) = do
        putWord8 MagicNumbers.constant_Float
        putWord32be bytes
    writeBinary (LongInfo{..}) = do
        putWord8 MagicNumbers.constant_Long
        putWord32be highBytes
        putWord32be lowBytes
    writeBinary (DoubleInfo{..}) = do
        putWord8 MagicNumbers.constant_Double
        putWord32be highBytes
        putWord32be lowBytes
    writeBinary (NameAndTypeInfo{..}) = do
        putWord8 MagicNumbers.constant_NameAndType
        putWord16be nameIndex
        putWord16be descriptorIndex
    writeBinary (UTF8Info{..}) = do
        putWord8 MagicNumbers.constant_Utf8
        putWord16be (fromIntegral $ BS.length utfBytes)
        putByteString utfBytes
    writeBinary (MethodHandleInfo{..}) = do
        putWord8 MagicNumbers.constant_MethodHandle
        putWord8 referenceKind
        putWord16be referenceIndex
    writeBinary (MethodTypeInfo{..}) = do
        putWord8 MagicNumbers.constant_MethodType
        putWord16be descriptorIndex
    writeBinary (InvokeDynamicInfo{..}) = do
        putWord8 MagicNumbers.constant_InvokeDynamic
        putWord16be bootstrapMethodAttrIndex
        putWord16be nameAndTypeIndex
