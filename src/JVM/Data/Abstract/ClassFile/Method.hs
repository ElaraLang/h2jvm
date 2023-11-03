module JVM.Data.Abstract.ClassFile.Method where

import Data.Text (Text)
import JVM.Data.Abstract.ClassFile.AccessFlags (MethodAccessFlag)
import JVM.Data.Abstract.Descriptor (MethodDescriptor)

import Data.Data
import Data.TypeMergingList (DataMergeable (merge), errorDifferentConstructors)
import Data.Void
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type (ClassInfoType)
import JVM.Data.Raw.Types (U2, U1)

data ClassFileMethod = ClassFileMethod
    { methodAccessFlags :: [MethodAccessFlag]
    , methodName :: Text
    , methodDescriptor :: MethodDescriptor
    , methodAttributes :: [MethodAttribute]
    }
    deriving (Show)

data MethodAttribute
    = Code !CodeAttributeData
    deriving (Show)

data CodeAttributeData = CodeAttributeData
    { maxStack :: U2
    , maxLocals :: U2
    , code :: [Instruction]
    , exceptionTable :: [ExceptionTableEntry]
    , codeAttributes :: [CodeAttribute]
    }
    deriving (Show)

data ExceptionTableEntry = ExceptionTableEntry
    { startPc :: Int
    , endPc :: Int
    , handlerPc :: Int
    , catchType :: Maybe ClassInfoType
    }
    deriving (Show)

data CodeAttribute
    = LineNumberTable [LineNumberTableEntry]
    | StackMapTable [StackMapFrame]
    deriving (Show, Eq, Data)

instance DataMergeable CodeAttribute where
    merge (LineNumberTable a) (LineNumberTable b) = LineNumberTable (a <> b)
    merge (StackMapTable a) (StackMapTable b) = StackMapTable (a <> b)
    merge x y = errorDifferentConstructors x y

data StackMapFrame
    = SameFrame Label
    | ChopFrame
        !U1 -- | How many locals to chop
        !Label -- | The label of the next instruction
    | SameLocals1StackItemFrame !VerificationTypeInfo Label
    deriving (Show, Data, Eq)

data  VerificationTypeInfo = TopVariableInfo
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
