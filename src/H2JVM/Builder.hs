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
    addMethodWithCode,
    addMethodWithCode_,
    addAttribute,
    addBootstrapMethod,
    runClassBuilder,
)
where

import Data.Text (Text)
import Data.Tuple (swap)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.TH

import H2JVM (CodeBuilder)
import H2JVM.Analyse.StackMap (StackMapError, calculateStackMapFrames)
import H2JVM.Builder.Code (runCodeBuilder)
import H2JVM.ClassFile (ClassFile (..), ClassFileAttribute (BootstrapMethods))
import H2JVM.ClassFile.AccessFlags (ClassAccessFlag, MethodAccessFlag)
import H2JVM.ClassFile.Field
import H2JVM.ClassFile.Method
import H2JVM.ConstantPool
import H2JVM.Descriptor (MethodDescriptor)
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

{- | Add a method definition to the class being built, whose body is the result of some 'CodeBuilder' monad.
This is essentially a convenience function to:
1. Run the 'CodeBuilder'
2. Calculate the 'StackMapTable' etc.
3. Setup the 'ClassFileMethod' and 'CodeAttributeData'
4. Add the method to this 'ClassBuilder'.
-}
addMethodWithCode :: (ClassBuilder :> r, Error StackMapError :> r, HasCallStack) => Text -> [MethodAccessFlag] -> MethodDescriptor -> Eff (CodeBuilder ': r) a -> Eff r a
addMethodWithCode methodName methodAccessFlags methodDescriptor codeBlock = do
    className <- getName
    (res, userCodeAttrs, instructions) <- runCodeBuilder codeBlock

    (frames, maxStack, maxLocals) <-
        case calculateStackMapFrames className methodAccessFlags methodDescriptor instructions of
            Left err -> throwError err
            Right res -> pure res

    let finalCodeAttrs = StackMapTable frames : userCodeAttrs
        codeData =
            CodeAttributeData
                { maxStack = fromIntegral maxStack
                , maxLocals = fromIntegral maxLocals
                , code = instructions
                , exceptionTable = [] -- TODO: this will be supported in more depth soon
                , codeAttributes = finalCodeAttrs
                }
        methodInfo =
            ClassFileMethod
                { methodAccessFlags = methodAccessFlags
                , methodName = methodName
                , methodDescriptor = methodDescriptor
                , methodAttributes = TML.fromList [Code codeData]
                }

    addMethod methodInfo
    pure res

-- | Like 'addMethodWithCode', but ignoring the result of the function.
addMethodWithCode_ :: (ClassBuilder :> r, Error StackMapError :> r, HasCallStack) => Text -> [MethodAccessFlag] -> MethodDescriptor -> Eff (CodeBuilder ': r) a -> Eff r ()
addMethodWithCode_ methodName methodAccessFlags methodDescriptor codeBlock = do
    _ <- addMethodWithCode methodName methodAccessFlags methodDescriptor codeBlock
    pure ()

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
