{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module JVM.Data.Raw.ClassFile where

import Data.Binary.Builder (toLazyByteString)
import Data.Binary.Put
import Data.Binary.Write (WriteBinary (writeBinary), writeList)
import Data.ByteString qualified as B
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import JVM.Data.JVMVersion (MajorVersion (..), MinorVersion, unwrapMajor, unwrapMinor)
import JVM.Data.Raw.ConstantPool (ConstantPoolInfo)
import JVM.Data.Raw.Instruction (Instruction)
import JVM.Data.Raw.MagicNumbers qualified as MagicNumbers
import JVM.Data.Raw.Types

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
    | InnerClassesAttribute (Vector InnerClassInfo)
    | LineNumberTableAttribute (Vector LineNumberTableEntry)
    | StackMapTableAttribute (Vector StackMapFrame)
    | ConstantValueAttribute Word16
    | SourceFileAttribute Word16
    | BootstrapMethodsAttribute (Vector BootstrapMethod)
    deriving (Show)

data InnerClassInfo = InnerClassInfo
    { innerClassInfo :: Word16
    , outerClassInfo :: Word16
    , innerName :: Word16
    , accessFlags :: Word16
    }
    deriving (Show)

data LineNumberTableEntry = LineNumberTableEntry
    { startPc :: U2
    , lineNumber :: U2
    }
    deriving (Show)

{- | The stack map frame types are defined as follows:
> union stack_map_frame {
>    same_frame;
>    same_locals_1_stack_item_frame;
>    same_locals_1_stack_item_frame_extended;
>    chop_frame;
>    same_frame_extended;
>    append_frame;
>    full_frame;
>}
-}
data StackMapFrame
    = -- | 0-63
      SameFrame U1
    | -- |
      -- >       same_locals_1_stack_item_frame {
      -- >          u1 frame_type = SAME_LOCALS_1_STACK_ITEM; /* 64-127 */
      -- >          verification_type_info stack[1];
      -- >      }
      SameLocals1StackItemFrame VerificationTypeInfo U1
    | SameLocals1StackItemFrameExtended VerificationTypeInfo U2
    | SameFrameExtended U2
    | ChopFrame U1 U2
    | AppendFrame (Vector VerificationTypeInfo) U2
    | FullFrame (Vector VerificationTypeInfo) (Vector VerificationTypeInfo) U2
    deriving (Show)

data VerificationTypeInfo
    = TopVariableInfo
    | IntegerVariableInfo
    | FloatVariableInfo
    | LongVariableInfo
    | DoubleVariableInfo
    | NullVariableInfo
    | UninitializedThisVariableInfo
    | ObjectVariableInfo Word16
    | UninitializedVariableInfo U2
    deriving (Show)

data BootstrapMethod = BootstrapMethod
    { bootstrapMethodRef :: Word16
    , bootstrapArguments :: Vector Word16
    }
    deriving (Show, Eq, Ord)

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

-- | Puts the attribute data. This must not include length or name, just the actual value
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
putAttribute (InnerClassesAttribute classes) = do
    putWord16be $ fromIntegral $ V.length classes
    mapM_ putInnerClassInfo classes
  where
    putInnerClassInfo :: InnerClassInfo -> Put
    putInnerClassInfo InnerClassInfo{..} = do
        putWord16be innerClassInfo
        putWord16be outerClassInfo
        putWord16be innerName
        putWord16be accessFlags
putAttribute (SourceFileAttribute sfIndex) = do
    putWord16be sfIndex
putAttribute (BootstrapMethodsAttribute bms) = do
    putWord16be $ fromIntegral $ V.length bms
    mapM_ putBootstrapMethod bms
putAttribute (LineNumberTableAttribute lns) = do
    putWord16be $ fromIntegral $ V.length lns
    mapM_ putLineNumberEntry lns
  where
    putLineNumberEntry :: LineNumberTableEntry -> Put
    putLineNumberEntry LineNumberTableEntry{..} = do
        putWord16be startPc
        putWord16be lineNumber
putAttribute (StackMapTableAttribute frames) = do
    putWord16be $ fromIntegral $ V.length frames
    mapM_ putStackMapFrame frames
  where
    putStackMapFrame :: StackMapFrame -> Put
    putStackMapFrame (SameFrame offset) = putWord8 offset
    putStackMapFrame (SameFrameExtended offset) = do
        putWord8 251
        putWord16be offset
    putStackMapFrame (SameLocals1StackItemFrame info offset) = do
        putWord8 (64 + offset) -- frame_type = 64 + offset
        putVerificationTypeInfo info
    putStackMapFrame (SameLocals1StackItemFrameExtended info offset) = do
        putWord8 247
        putWord16be offset
        putVerificationTypeInfo info
    putStackMapFrame (ChopFrame chop label) = do
        putWord8 (251 - chop) -- chop = 251 - frame_type => frame_type = 251 - chop
        putWord16be label
    putStackMapFrame (AppendFrame infos offset) = do
        putWord8 (251 + fromIntegral (V.length infos)) -- frame_type = 251 + number_of_locals
        putWord16be offset
        mapM_ putVerificationTypeInfo infos
    putStackMapFrame (FullFrame locals stack offset) = do
        putWord8 255
        putWord16be offset
        putWord16be (fromIntegral $ V.length locals)
        mapM_ putVerificationTypeInfo locals
        putWord16be (fromIntegral $ V.length stack)
        mapM_ putVerificationTypeInfo stack

    putVerificationTypeInfo :: VerificationTypeInfo -> Put
    putVerificationTypeInfo TopVariableInfo = putWord8 MagicNumbers.verificationType_info_TopVariableInfo
    putVerificationTypeInfo IntegerVariableInfo = putWord8 MagicNumbers.verificationType_info_IntegerVariableInfo
    putVerificationTypeInfo FloatVariableInfo = putWord8 MagicNumbers.verificationType_info_FloatVariableInfo
    putVerificationTypeInfo LongVariableInfo = putWord8 MagicNumbers.verificationType_info_LongVariableInfo
    putVerificationTypeInfo DoubleVariableInfo = putWord8 MagicNumbers.verificationType_info_DoubleVariableInfo
    putVerificationTypeInfo NullVariableInfo = putWord8 MagicNumbers.verificationType_info_NullVariableInfo
    putVerificationTypeInfo UninitializedThisVariableInfo = putWord8 MagicNumbers.verificationType_info_UninitializedThisVariableInfo
    putVerificationTypeInfo (ObjectVariableInfo index) = do
        putWord8 MagicNumbers.verificationType_info_ObjectVariableInfo
        putWord16be index
    putVerificationTypeInfo (UninitializedVariableInfo offset) = do
        putWord8 MagicNumbers.verificationType_info_UninitializedVariableInfo
        putWord16be offset

putBootstrapMethod :: BootstrapMethod -> Put
putBootstrapMethod BootstrapMethod{..} = do
    putWord16be bootstrapMethodRef
    putWord16be $ fromIntegral $ V.length bootstrapArguments
    mapM_ putWord16be bootstrapArguments

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
        putWord16be (unwrapMinor minorVersion)
        putWord16be (unwrapMajor majorVersion)
        putConstantPool constantPool
        putWord16be accessFlags
        putWord16be thisClass
        putWord16be superClass
        putInterfaceTable interfaces

        putFieldTable fields
        putMethodTable methods
        putAttributes attributes
