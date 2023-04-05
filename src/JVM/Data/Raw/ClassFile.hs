{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Raw.ClassFile where

import Data.Binary.Builder (toLazyByteString)
import Data.Binary.Put
import Data.Binary.Write (WriteBinary (writeBinary), writeList)
import Data.ByteString qualified as B
import Data.Coerce
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import JVM.Data.JVMVersion (MajorVersion (..), MinorVersion (getMinorVersion))
import JVM.Data.Raw.ConstantPool (ConstantPoolInfo)
import JVM.Data.Raw.Instruction (Instruction)

{-
ClassFile {
    u4             magic;
    u2             minor_version;
    u2             major_version;
    u2             constant_pool_count;
    cp_info        constant_pool[constant_pool_count-1];
    u2             access_flags;
    u2             this_class;
    u2             super_class;
    u2             interfaces_count;
    u2             interfaces[interfaces_count];
    u2             fields_count;
    field_info     fields[fields_count];
    u2             methods_count;
    method_info    methods[methods_count];
    u2             attributes_count;
    attribute_info attributes[attributes_count];
}
-}

{- | Low level representation of the class file format.
This is the closest representation to the actual format of the class file, that makes only a few compromises for convenience,
such as omitting the explicit x_count fields, instead calculating them from the length of the corresponding vectors.
This has the added bonus of making it impossible to construct an invalid class file (in this regard anyway)
-}
data ClassFile = ClassFile
    { magic :: Word32
    , minorVersion :: MinorVersion
    , majorVersion :: MajorVersion
    , constantPool :: Vector ConstantPoolInfo
    , accessFlags :: Word16
    , thisClass :: Word16
    , superClass :: Word16
    , interfaces :: Vector Word16
    , fields :: Vector FieldInfo
    , methods :: Vector MethodInfo
    , attributes :: Vector AttributeInfo
    }
    deriving (Show)

data FieldInfo = FieldInfo
    { accessFlags :: Word16
    , nameIndex :: Word16
    , descriptorIndex :: Word16
    , attributes :: Vector AttributeInfo
    }
    deriving (Show)

data AttributeInfo = AttributeInfo
    { nameIndex :: Word16
    , info :: Attribute
    }
    deriving (Show)

data Attribute
    = CodeAttribute
        { maxStack :: Word16
        , maxLocals :: Word16
        , code :: Vector Instruction
        , exceptionTable :: Vector ExceptionTableEntry
        , codeAttributes :: Vector AttributeInfo
        }
    | ConstantValueAttribute Word16
    deriving (Show)

data ExceptionTableEntry = ExceptionTableEntry
    { startPc :: Word16
    , endPc :: Word16
    , handlerPc :: Word16
    , catchType :: Word16
    }
    deriving (Show)

data MethodInfo = MethodInfo
    { accessFlags :: Word16
    , nameIndex :: Word16
    , descriptorIndex :: Word16
    , attributes :: Vector AttributeInfo
    }
    deriving (Show)

putConstantPool :: V.Vector ConstantPoolInfo -> Put
putConstantPool entries = do
    putWord16be $ fromIntegral (length entries + 1)
    mapM_ writeBinary entries

putInterfaceTable :: Vector Word16 -> Put
putInterfaceTable = writeList putWord16be

putFieldTable :: Vector FieldInfo -> Put
putFieldTable = writeList putWord16be

instance WriteBinary FieldInfo where
    writeBinary FieldInfo{..} = do
        putWord16be accessFlags
        putWord16be nameIndex
        putWord16be descriptorIndex
        putAttributes attributes

putAttributes :: Vector AttributeInfo -> Put
putAttributes = writeList putWord16be

instance WriteBinary AttributeInfo where
    writeBinary AttributeInfo{..} = do
        putWord16be nameIndex
        let infoBS = B.toStrict $ runPut $ putAttribute info
        let aiLength = B.length infoBS
        putWord32be $ fromIntegral aiLength
        putByteString infoBS

-- Puts the attribute data. This must not include length or name, just the actual value
putAttribute :: Attribute -> Put
putAttribute (ConstantValueAttribute cvIndex) = putWord16be cvIndex
putAttribute (CodeAttribute maxStack maxLocals code exceptionTable codeAttributes) = do
    putWord16be maxStack
    putWord16be maxLocals
    let codeStr = B.toStrict $ toLazyByteString $ execPut $ mapM_ writeBinary code
    putWord32be $ fromIntegral $ B.length codeStr
    putByteString codeStr
    putExceptionTable exceptionTable
    putAttributes codeAttributes

putExceptionTable :: Vector ExceptionTableEntry -> Put
putExceptionTable = writeList putWord16be

instance WriteBinary ExceptionTableEntry where
    writeBinary (ExceptionTableEntry{..}) = do
        putWord16be startPc
        putWord16be endPc
        putWord16be handlerPc
        putWord16be catchType

putMethodTable :: Vector MethodInfo -> Put
putMethodTable = writeList putWord16be

instance WriteBinary MethodInfo where
    writeBinary MethodInfo{..} = do
        putWord16be accessFlags
        putWord16be nameIndex
        putWord16be descriptorIndex
        putAttributes attributes

instance WriteBinary ClassFile where
    writeBinary ClassFile{..} = do
        putWord32be magic
        putWord16be (getMinorVersion minorVersion)
        putWord16be (getMajorVersion majorVersion)
        putConstantPool constantPool
        putWord16be accessFlags
        putWord16be thisClass
        putWord16be superClass
        putInterfaceTable interfaces

        putFieldTable fields
        putMethodTable methods
        putAttributes attributes