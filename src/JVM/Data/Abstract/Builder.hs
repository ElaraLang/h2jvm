{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | Provides a monadic interface for building class files in a high-level format.
module JVM.Data.Abstract.Builder where

import Control.Monad.State
import Data.Functor.Identity
import JVM.Data.Abstract.ClassFile (ClassFile (..), ClassFileAttribute (BootstrapMethods), methods)
import JVM.Data.Abstract.ClassFile.AccessFlags (ClassAccessFlag)
import JVM.Data.Abstract.ClassFile.Field
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Abstract.Name
import JVM.Data.JVMVersion

newtype ClassBuilderT m a = ClassBuilderT {unClassBuilderT :: StateT ClassFile m a}
    deriving newtype (Functor, Applicative, Monad, MonadState ClassFile, MonadTrans)


type ClassBuilder = ClassBuilderT Identity

addAccessFlag :: Monad m => ClassAccessFlag -> ClassBuilderT m ()
addAccessFlag flag = modify (\c -> c{accessFlags = flag : accessFlags c})

setName :: Monad m => QualifiedClassName -> ClassBuilderT m ()
setName n = modify (\c -> c{name = n})

setVersion :: Monad m => JVMVersion -> ClassBuilderT m ()
setVersion v = modify (\c -> c{version = v})

setSuperClass :: Monad m => QualifiedClassName -> ClassBuilderT m ()
setSuperClass s = modify (\c -> c{superClass = Just s})

addInterface :: Monad m => QualifiedClassName -> ClassBuilderT m ()
addInterface i = modify (\c -> c{interfaces = i : interfaces c})

addField :: Monad m => ClassFileField -> ClassBuilderT m ()
addField f = modify (\c -> c{fields = f : fields c})

buildAndAddField :: Monad m => ClassBuilderT m ClassFileField -> ClassBuilderT m ()
buildAndAddField f = f >>= addField

addMethod :: Monad m => ClassFileMethod -> ClassBuilderT m ()
addMethod m = modify (\c -> c{methods = m : methods c})

buildAndAddMethod :: Monad m => ClassBuilderT m ClassFileMethod -> ClassBuilderT m ()
buildAndAddMethod m = m >>= addMethod

addAttribute :: Monad m => ClassFileAttribute -> ClassBuilderT m ()
addAttribute a = modify (\c -> c{attributes = a : attributes c})

addBootstrapMethod :: Monad m => BootstrapMethod -> ClassBuilderT m ()
addBootstrapMethod b = modify (\c -> c{attributes = mergeAttributes (attributes c)})
  where
    mergeAttributes :: [ClassFileAttribute] -> [ClassFileAttribute]
    mergeAttributes [] = []
    mergeAttributes (BootstrapMethods bs : as) = BootstrapMethods (b : bs) : mergeAttributes as
    mergeAttributes (a : as) = a : mergeAttributes as

dummyClass :: QualifiedClassName -> JVMVersion -> ClassFile
dummyClass name version =
    ClassFile
        { name = name
        , version = version
        , accessFlags = []
        , superClass = Nothing
        , interfaces = []
        , fields = []
        , methods = []
        , attributes = []
        }

runClassBuilderT :: Monad m => QualifiedClassName -> JVMVersion -> ClassBuilderT m a -> m (a, ClassFile)
runClassBuilderT n v m =
    runStateT (unClassBuilderT m) (dummyClass n v)

runClassBuilder :: QualifiedClassName -> JVMVersion -> ClassBuilder a -> (a, ClassFile)
runClassBuilder n v m =
    runIdentity $ runClassBuilderT n v m
