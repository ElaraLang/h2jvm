module JVM.Data.Abstract.Method where

import Data.Text (Text)
import JVM.Data.Abstract.AccessFlags (MethodAccessFlag)
import JVM.Data.Abstract.Type (ClassInfoType, FieldType)
import JVM.Data.Raw.Instruction (Instruction)

data ClassFileMethod = ClassFileMethod
    { methodAccessFlags :: [MethodAccessFlag]
    , methodName :: Text
    , methodDescriptor :: MethodDescriptor
    , methodAttributes :: [MethodAttribute]
    }
    deriving (Show)

data MethodDescriptor
    = MethodDescriptor [FieldType] ReturnDescriptor
    deriving (Show)

data ReturnDescriptor
    = VoidReturn
    | Return FieldType
    deriving (Show)

data MethodAttribute
    = Code !CodeAttributeData
    deriving (Show)

data CodeAttributeData = CodeAttributeData
    { maxStack :: Int
    , maxLocals :: Int
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

newtype CodeAttribute
    = LineNumberTable [LineNumberTableEntry]
    deriving (Show)

data LineNumberTableEntry = LineNumberTableEntry
    { lineNumberTableEntryStartPc :: Int
    , lineNumberTableEntryLineNumber :: Int
    }
    deriving (Show)
