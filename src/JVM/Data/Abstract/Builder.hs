{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | Provides a monadic interface for building classfiles in a high-level format.
module JVM.Data.Abstract.Builder where

import Control.Monad.State
import Data.Functor.Identity
import JVM.Data.Abstract.AccessFlags (ClassAccessFlag)
import JVM.Data.Abstract.ClassFile (ClassFile (..), ClassFileAttribute, methods)
import JVM.Data.Abstract.Field
import JVM.Data.Abstract.Method
import JVM.Data.Abstract.Name
import JVM.Data.JVMVersion

newtype ClassBuilderT m a = ClassBuilderT {runClassBuilderT :: StateT ClassFile m a}
    deriving newtype (Functor, Applicative, Monad, MonadState ClassFile, MonadTrans)

type ClassBuilder a = ClassBuilderT Identity a

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

runClassBuilder :: Monad m => ClassBuilderT m a -> QualifiedClassName -> JVMVersion -> m (a, ClassFile)
runClassBuilder m n v =
    runStateT (runClassBuilderT m) (dummyClass n v)