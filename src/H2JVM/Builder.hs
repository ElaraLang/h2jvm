{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides a monadic interface for building class files in a high-level format.
module H2JVM.Builder (
    ClassBuilder,
    addAccessFlag,
    setName,
    getName,
    setVersion,
    setSuperClass,
    addInterface,
    addField,
    addMethod,
    addAttribute,
    addBootstrapMethod,
    runClassBuilder,
)
where

import Data.Tuple (swap)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH

import H2JVM.ClassFile (ClassFile (..), ClassFileAttribute (BootstrapMethods))
import H2JVM.ClassFile.AccessFlags (ClassAccessFlag)
import H2JVM.ClassFile.Field
import H2JVM.ClassFile.Method
import H2JVM.ConstantPool
import H2JVM.JVMVersion
import H2JVM.Name

import H2JVM.Data.TypeMergingList qualified as TML

-- | The class builder effect for constructing a 'ClassFile' statefully.
data ClassBuilder m a where
    ModifyClass :: (ClassFile -> ClassFile) -> ClassBuilder m ()
    GetClass :: ClassBuilder m ClassFile

makeEffect ''ClassBuilder

-- | Add an access flag modifier to the class being built.
addAccessFlag :: ClassBuilder :> r => ClassAccessFlag -> Eff r ()
addAccessFlag flag = modifyClass (\c -> c{accessFlags = flag : c.accessFlags})

-- | Set the fully qualified name of the class being built.
setName :: ClassBuilder :> r => QualifiedClassName -> Eff r ()
setName n = modifyClass (\c -> c{name = n})

-- | Retrieve the current fully qualified name of the class being built.
getName :: ClassBuilder :> r => Eff r QualifiedClassName
getName = (.name) <$> getClass

-- | Set the target JVM version of the class being built.
setVersion :: ClassBuilder :> r => JVMVersion -> Eff r ()
setVersion v = modifyClass (\c -> c{version = v})

-- | Set the superclass name of the class being built.
setSuperClass :: ClassBuilder :> r => QualifiedClassName -> Eff r ()
setSuperClass s = modifyClass (\c -> c{superClass = Just s})

-- | Add an implemented interface to the class being built.
addInterface :: ClassBuilder :> r => QualifiedClassName -> Eff r ()
addInterface i = modifyClass (\c -> c{interfaces = i : c.interfaces})

-- | Add a field definition to the class being built.
addField :: ClassBuilder :> r => ClassFileField -> Eff r ()
addField f = modifyClass (\c -> c{fields = f : c.fields})

-- | Add a method definition to the class being built.
addMethod :: ClassBuilder :> r => ClassFileMethod -> Eff r ()
addMethod m = modifyClass (\c -> c{methods = m : c.methods})

-- | Add a class file attribute to the class being built.
addAttribute :: ClassBuilder :> r => ClassFileAttribute -> Eff r ()
addAttribute a = modifyClass (\c -> c{attributes = c.attributes `TML.snoc` a})

-- | Add a bootstrap method for @invokedynamic@ calls.
addBootstrapMethod :: ClassBuilder :> r => BootstrapMethod -> Eff r ()
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

classBuilderToState :: State ClassFile :> r => Eff (ClassBuilder ': r) a -> Eff r a
classBuilderToState = interpret $ \_ -> \case
    ModifyClass f -> modify f
    GetClass -> get

{- | Run a class builder effect, returning the completed 'ClassFile' along with the result of the action
Any unset attributes in the class will be 'mempty' or equivalent, except 'name' and 'version' which must be present.
-}
runClassBuilder :: QualifiedClassName -> JVMVersion -> Eff (ClassBuilder : r) a -> Eff r (ClassFile, a)
runClassBuilder n v =
    fmap
        (fmap swap)
        ( runState (dummyClass n v)
            . classBuilderToState
            . inject
        )
