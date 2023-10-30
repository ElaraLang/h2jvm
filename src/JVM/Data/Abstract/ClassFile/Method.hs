module JVM.Data.Abstract.ClassFile.Method where

import Data.Text (Text)
import JVM.Data.Abstract.ClassFile.AccessFlags (MethodAccessFlag)
import JVM.Data.Abstract.Descriptor (MethodDescriptor)

import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type (ClassInfoType)
import JVM.Data.Raw.Types (U2)

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
    deriving (Show)

data StackMapFrame
    = SameFrame
    deriving (Show)

data LineNumberTableEntry = LineNumberTableEntry
    { lineNumberTableEntryStartPc :: U2
    , lineNumberTableEntryLineNumber :: U2
    }
    deriving (Show)
