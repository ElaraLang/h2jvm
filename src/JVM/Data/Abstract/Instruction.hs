{-# LANGUAGE DataKinds #-}

-- | High level representation of a JVM instruction, with type-safe arguments and no stack manipulation needed.
module JVM.Data.Abstract.Instruction where

import Data.Text (Text)
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Type (ClassInfoType, FieldType)

type Reference = Int

data Instruction
    = ALoad0
    | ALoad1
    | ALoad2
    | ALoad3
    | AReturn
    | AThrow
    | AConstNull
    | InvokeStatic ClassInfoType Text MethodDescriptor
    | InvokeVirtual ClassInfoType Text MethodDescriptor
    | InvokeDynamic BootstrapMethod Text MethodDescriptor
    | LDC LDCEntry
    | PutStatic ClassInfoType Text FieldType
    | GetStatic ClassInfoType Text FieldType
    | CheckCast ClassInfoType
    | Return
    deriving (Show, Eq, Ord)

data LDCEntry
    = LDCInt Int
    | LDCFloat Float
    | LDCString Text
    | LDCClass ClassInfoType
    deriving (Show, Eq, Ord)
