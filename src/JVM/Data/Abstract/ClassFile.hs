-- | High level representation of a class file
module JVM.Data.Abstract.ClassFile where

import JVM.Data.Abstract.AccessFlags (ClassAccessFlag)
import JVM.Data.Abstract.JVMVersion (JVMVersion)
import JVM.Data.Abstract.Name

data ClassFile = ClassFile
    { name :: QualifiedClassName
    , version :: JVMVersion
    , accessFlags :: [ClassAccessFlag]
    , superClass :: Maybe QualifiedClassName
    , interfaces :: [QualifiedClassName]
    }

