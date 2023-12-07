module JVM.Data.Abstract.ClassFile.Method where

import Data.Text (Text)
import JVM.Data.Abstract.ClassFile.AccessFlags (MethodAccessFlag)
import JVM.Data.Abstract.Descriptor (MethodDescriptor)

import Data.Data
import Data.TypeMergingList (DataMergeable (merge), TypeMergingList, errorDifferentConstructors)
import GHC.Generics (Generic)
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type (ClassInfoType)
import JVM.Data.Raw.Types (U1, U2)

data ClassFileMethod = ClassFileMethod
    { methodAccessFlags :: [MethodAccessFlag]
    , methodName :: Text
    , methodDescriptor :: MethodDescriptor
    , methodAttributes :: TypeMergingList MethodAttribute
    }
    deriving (Show)

data MethodAttribute
    = Code !CodeAttributeData
    deriving (Show, Generic, Data)

data CodeAttributeData = CodeAttributeData
    { maxStack :: U2
    , maxLocals :: U2
    , code :: [Instruction]
    , exceptionTable :: [ExceptionTableEntry]
    , codeAttributes :: [CodeAttribute]
    }
    deriving (Show, Data, Generic)

data ExceptionTableEntry = ExceptionTableEntry
    { startPc :: Int
    , endPc :: Int
    , handlerPc :: Int
    , catchType :: Maybe ClassInfoType
    }
    deriving (Show, Data, Generic)

data CodeAttribute
    = LineNumberTable [LineNumberTableEntry]
    | StackMapTable [StackMapFrame]
    deriving (Show, Eq, Data, Generic)

instance DataMergeable CodeAttribute where
    merge (LineNumberTable a) (LineNumberTable b) = LineNumberTable (a <> b)
    merge (StackMapTable a) (StackMapTable b) = StackMapTable (a <> b)
    merge x y = errorDifferentConstructors x y

instance DataMergeable MethodAttribute where
    merge (Code a) (Code b) = Code (merge a b)
    merge x y = errorDifferentConstructors x y

instance DataMergeable CodeAttributeData where
    merge (CodeAttributeData a b c d e) (CodeAttributeData a' b' c' d' e') =
        CodeAttributeData (max a a') (max b b') (c <> c') (d <> d') (e <> e')

data StackMapFrame
    = SameFrame Label
    | ChopFrame
        -- | How many locals to chop
        !U1
        -- | The label of the next instruction
        !Label
    | SameLocals1StackItemFrame !VerificationTypeInfo Label
    | AppendFrame ![VerificationTypeInfo] !Label
    | FullFrame ![VerificationTypeInfo] ![VerificationTypeInfo] !Label
    deriving (Show, Data, Eq)

data VerificationTypeInfo
    = TopVariableInfo
    | IntegerVariableInfo
    | FloatVariableInfo
    | LongVariableInfo
    | DoubleVariableInfo
    | NullVariableInfo
    | UninitializedThisVariableInfo
    | ObjectVariableInfo !ClassInfoType
    | UninitializedVariableInfo !Label
    deriving (Show, Data, Eq)

data LineNumberTableEntry = LineNumberTableEntry
    { lineNumberTableEntryStartPc :: U2
    , lineNumberTableEntryLineNumber :: U2
    }
    deriving (Show, Data, Eq)
