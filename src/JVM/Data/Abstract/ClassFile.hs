{-# LANGUAGE DuplicateRecordFields #-}

-- | High level representation of a class file
module JVM.Data.Abstract.ClassFile where

import Data.Text (Text)
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
    , attributes :: [ClassFileAttribute]
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
    deriving (Show)

data InnerClassInfo = InnerClassInfo
    { innerClassInfo :: QualifiedClassName
    , outerClassInfo :: QualifiedClassName
    , innerName :: Text
    , accessFlags :: [ClassAccessFlag]
    }
    deriving (Show)
