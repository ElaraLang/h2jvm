{-# LANGUAGE DuplicateRecordFields #-}

-- | High level representation of a class file
module JVM.Data.Abstract.ClassFile where

import Data.Text (Text)
import JVM.Data.Abstract.AccessFlags (ClassAccessFlag)
import JVM.Data.Abstract.Name
import JVM.Data.JVMVersion (JVMVersion)

data ClassFile = ClassFile
    { name :: QualifiedClassName
    , version :: JVMVersion
    , accessFlags :: [ClassAccessFlag]
    , superClass :: Maybe QualifiedClassName
    , interfaces :: [QualifiedClassName]
    , attributes :: [ClassFileAttribute]
    }

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
    | BootstrapMethods

data InnerClassInfo = InnerClassInfo
    { innerClassInfo :: QualifiedClassName
    , outerClassInfo :: QualifiedClassName
    , innerName :: Text
    , accessFlags :: [ClassAccessFlag]
    }
