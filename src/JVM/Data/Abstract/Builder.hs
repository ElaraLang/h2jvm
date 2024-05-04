{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides a monadic interface for building class files in a high-level format.
module JVM.Data.Abstract.Builder where

import Data.TypeMergingList qualified as TML
import JVM.Data.Abstract.ClassFile (ClassFile (..), ClassFileAttribute (BootstrapMethods), methods, InnerClassInfo)
import JVM.Data.Abstract.ClassFile.AccessFlags (ClassAccessFlag)
import JVM.Data.Abstract.ClassFile.Field
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Abstract.Name
import JVM.Data.JVMVersion
import Polysemy
import Polysemy.State

data ClassBuilder m a where
    ModifyClass :: (ClassFile -> ClassFile) -> ClassBuilder m ()
    GetClass :: ClassBuilder m ClassFile

makeSem ''ClassBuilder

addAccessFlag :: (Member ClassBuilder r) => ClassAccessFlag -> Sem r ()
addAccessFlag flag = modifyClass (\c -> c{accessFlags = flag : c.accessFlags})

setName :: (Member ClassBuilder r) => QualifiedClassName -> Sem r ()
setName n = modifyClass (\c -> c{name = n})

getName :: (Member ClassBuilder r) => Sem r QualifiedClassName
getName = (.name) <$> getClass

setVersion :: (Member ClassBuilder r) => JVMVersion -> Sem r ()
setVersion v = modifyClass (\c -> c{version = v})

setSuperClass :: (Member ClassBuilder r) => QualifiedClassName -> Sem r ()
setSuperClass s = modifyClass (\c -> c{superClass = Just s})

addInterface :: (Member ClassBuilder r) => QualifiedClassName -> Sem r ()
addInterface i = modifyClass (\c -> c{interfaces = i : c.interfaces})

addField :: (Member ClassBuilder r) => ClassFileField -> Sem r ()
addField f = modifyClass (\c -> c{fields = f : c.fields})

addMethod :: (Member ClassBuilder r) => ClassFileMethod -> Sem r ()
addMethod m = modifyClass (\c -> c{methods = m : c.methods})

addAttribute :: (Member ClassBuilder r) => ClassFileAttribute -> Sem r ()
addAttribute a = modifyClass (\c -> c{attributes = c.attributes `TML.snoc` a})

addBootstrapMethod :: (Member ClassBuilder r) => BootstrapMethod -> Sem r ()
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

classBuilderToState :: (Member (State ClassFile) r) => Sem (ClassBuilder ': r) a -> Sem r a
classBuilderToState = interpret $ \case
    ModifyClass f -> modify f
    GetClass -> get

runClassBuilder :: QualifiedClassName -> JVMVersion -> Sem (ClassBuilder : r) a -> Sem r (ClassFile, a)
runClassBuilder n v =
    runState (dummyClass n v)
        . classBuilderToState
        . raiseUnder
