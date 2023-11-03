{-# LANGUAGE DuplicateRecordFields #-}

-- | High level representation of a class file
module JVM.Data.Abstract.ClassFile where

import Data.Data
import Data.Text (Text)
import Data.TypeMergingList (DataMergeable (merge), TypeMergingList, errorDifferentConstructors)
import JVM.Data.Abstract.ClassFile.AccessFlags (ClassAccessFlag)
import JVM.Data.Abstract.ClassFile.Field (ClassFileField)
import JVM.Data.Abstract.ClassFile.Method (ClassFileMethod)
import JVM.Data.Abstract.ConstantPool (BootstrapMethod)
import JVM.Data.Abstract.Name
import JVM.Data.JVMVersion (JVMVersion)

data ClassFile = ClassFile
    { name :: QualifiedClassName
    , version :: JVMVersion
    , accessFlags :: [ClassAccessFlag]
    , superClass :: Maybe QualifiedClassName
    , interfaces :: [QualifiedClassName]
    , fields :: [ClassFileField]
    , methods :: [ClassFileMethod]
    , attributes :: TypeMergingList ClassFileAttribute
    }
    deriving (Show)

data ClassFileAttribute
    = InnerClasses [InnerClassInfo]
    | EnclosingMethod
    | Synthetic
    | Signature
    | SourceFile Text
    | SourceDebugExtension
    | Deprecated
    | RuntimeVisibleAnnotations
    | RuntimeInvisibleAnnotations
    | BootstrapMethods [BootstrapMethod]
    deriving (Show, Eq, Data)

instance DataMergeable ClassFileAttribute where
    merge (InnerClasses a) (InnerClasses b) = InnerClasses (a <> b)
    merge EnclosingMethod EnclosingMethod = EnclosingMethod
    merge Synthetic Synthetic = Synthetic
    merge Signature Signature = Signature
    merge (SourceFile a) (SourceFile b) = SourceFile (a <> b)
    merge SourceDebugExtension SourceDebugExtension = SourceDebugExtension
    merge Deprecated Deprecated = Deprecated
    merge RuntimeVisibleAnnotations RuntimeVisibleAnnotations = RuntimeVisibleAnnotations
    merge RuntimeInvisibleAnnotations RuntimeInvisibleAnnotations = RuntimeInvisibleAnnotations
    merge (BootstrapMethods a) (BootstrapMethods b) = BootstrapMethods (a <> b)
    merge a b = errorDifferentConstructors a b

data InnerClassInfo = InnerClassInfo
    { innerClassInfo :: QualifiedClassName
    , outerClassInfo :: QualifiedClassName
    , innerName :: Text
    , accessFlags :: [ClassAccessFlag]
    }
    deriving (Show, Eq, Data)
