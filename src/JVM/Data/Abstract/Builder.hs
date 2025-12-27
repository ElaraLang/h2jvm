{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides a monadic interface for building class files in a high-level format.
module JVM.Data.Abstract.Builder where

import Data.Tuple (swap)
import Data.TypeMergingList qualified as TML
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH
import JVM.Data.Abstract.ClassFile (ClassFile (..), ClassFileAttribute (BootstrapMethods))
import JVM.Data.Abstract.ClassFile.AccessFlags (ClassAccessFlag)
import JVM.Data.Abstract.ClassFile.Field
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Abstract.Name
import JVM.Data.JVMVersion

data ClassBuilder m a where
    ModifyClass :: (ClassFile -> ClassFile) -> ClassBuilder m ()
    GetClass :: ClassBuilder m ClassFile

makeEffect ''ClassBuilder

addAccessFlag :: (ClassBuilder :> r) => ClassAccessFlag -> Eff r ()
addAccessFlag flag = modifyClass (\c -> c{accessFlags = flag : c.accessFlags})

setName :: (ClassBuilder :> r) => QualifiedClassName -> Eff r ()
setName n = modifyClass (\c -> c{name = n})

getName :: (ClassBuilder :> r) => Eff r QualifiedClassName
getName = (.name) <$> getClass

setVersion :: (ClassBuilder :> r) => JVMVersion -> Eff r ()
setVersion v = modifyClass (\c -> c{version = v})

setSuperClass :: (ClassBuilder :> r) => QualifiedClassName -> Eff r ()
setSuperClass s = modifyClass (\c -> c{superClass = Just s})

addInterface :: (ClassBuilder :> r) => QualifiedClassName -> Eff r ()
addInterface i = modifyClass (\c -> c{interfaces = i : c.interfaces})

addField :: (ClassBuilder :> r) => ClassFileField -> Eff r ()
addField f = modifyClass (\c -> c{fields = f : c.fields})

addMethod :: (ClassBuilder :> r) => ClassFileMethod -> Eff r ()
addMethod m = modifyClass (\c -> c{methods = m : c.methods})

addAttribute :: (ClassBuilder :> r) => ClassFileAttribute -> Eff r ()
addAttribute a = modifyClass (\c -> c{attributes = c.attributes `TML.snoc` a})

addBootstrapMethod :: (ClassBuilder :> r) => BootstrapMethod -> Eff r ()
addBootstrapMethod b = addAttribute (BootstrapMethods [b])

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
        , attributes = mempty
        }

classBuilderToState :: ((State ClassFile) :> r) => Eff (ClassBuilder ': r) a -> Eff r a
classBuilderToState = interpret $ \_ -> \case
    ModifyClass f -> modify f
    GetClass -> get

runClassBuilder :: QualifiedClassName -> JVMVersion -> Eff (ClassBuilder : r) a -> Eff r (ClassFile, a)
runClassBuilder n v =
    fmap
        (fmap swap)
        ( runState (dummyClass n v)
            . classBuilderToState
            . inject
        )
