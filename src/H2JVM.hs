{-# LANGUAGE DuplicateRecordFields #-}

-- | Unified entry point for the H2JVM library.
module H2JVM (
    -- * Core Types
    ClassFile (..),
    ClassFileAttribute (
        InnerClasses,
        EnclosingMethod,
        Signature,
        SourceFile,
        SourceDebugExtension,
        Deprecated,
        RuntimeVisibleAnnotations,
        RuntimeInvisibleAnnotations,
        BootstrapMethods
    ),
    InnerClassInfo (..),
    JVMVersion,
    MajorVersion,
    MinorVersion,
    unwrapMajor,
    unwrapMinor,
    getMajor,
    getMinor,
    java6,
    java7,
    java8,
    java9,
    java10,
    java11,
    java12,
    java13,
    java14,
    java15,
    java16,
    java17,
    java18,
    java19,
    java20,
    java21,
    ClassAccessFlag (..),
    MethodAccessFlag (..),
    ClassFileField (..),
    ClassFileMethod (..),

    -- * Names, Types and Descriptors
    QualifiedClassName,
    PackageName,
    ClassName,
    parseQualifiedClassName,
    parseClassName,
    parsePackageName,
    classFilePath,
    MethodDescriptor (..),
    ReturnDescriptor (..),
    FieldType (..),
    PrimitiveType (..),

    -- * Constant Pool
    ConstantPoolEntry (..),
    MethodHandleEntry (..),
    FieldRef (..),
    MethodRef (..),
    BootstrapMethod (..),
    BootstrapArgument (..),

    -- * Code Generation & Instructions
    CodeBuilder,
    runCodeBuilder,
    emit,
    newLabel,
    Label,
    Instruction,
    Instruction' (..),
    LDCEntry (..),

    -- * High-level Serialization API
    classFileBytes,
    CodeConverterError,
) where

import Data.Binary.Put (runPut)
import Data.ByteString.Lazy (ByteString)

import H2JVM.Builder.Code (CodeBuilder, emit, newLabel, runCodeBuilder)
import H2JVM.Builder.Label (Label)
import H2JVM.ClassFile (ClassFile (..), ClassFileAttribute (..), InnerClassInfo (..))
import H2JVM.ClassFile.AccessFlags (ClassAccessFlag (..), MethodAccessFlag (..))
import H2JVM.ClassFile.Field (ClassFileField (..))
import H2JVM.ClassFile.Method (ClassFileMethod (..))
import H2JVM.ConstantPool (BootstrapArgument (..), BootstrapMethod (..), ConstantPoolEntry (..), FieldRef (..), MethodHandleEntry (..), MethodRef (..))
import H2JVM.Descriptor (MethodDescriptor (..), ReturnDescriptor (..))
import H2JVM.Instruction (Instruction, Instruction' (..), LDCEntry (..))
import H2JVM.Internal.Binary.Write (writeBinary)
import H2JVM.Internal.Convert (convert)
import H2JVM.Internal.Convert.Monad (CodeConverterError)
import H2JVM.JVMVersion (JVMVersion, MajorVersion, MinorVersion, getMajor, getMinor, java10, java11, java12, java13, java14, java15, java16, java17, java18, java19, java20, java21, java6, java7, java8, java9, unwrapMajor, unwrapMinor)
import H2JVM.Name (ClassName, PackageName, QualifiedClassName, parseClassName, parsePackageName, parseQualifiedClassName, suitableFilePath)
import H2JVM.Type (FieldType (..), PrimitiveType (..))

-- | Convert and serialize a high-level 'ClassFile' directly to lazy 'ByteString' bytes.
classFileBytes :: ClassFile -> Either CodeConverterError ByteString
classFileBytes = fmap (runPut . writeBinary) . convert

-- | Determine the standard relative output filepath for a class (e.g., @"java/lang/Object.class"@).
classFilePath :: ClassFile -> FilePath
classFilePath cf = suitableFilePath cf.name
