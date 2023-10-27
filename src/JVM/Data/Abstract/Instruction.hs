{-# LANGUAGE DataKinds #-}

{- | High level representation of a JVM instruction, with type-safe arguments and no stack manipulation needed.
 This is not a 1-1 mapping to the actual instructions, use 'JVM.Data.Raw.Instruction' for that.
 Instead, this includes some conveniences like the 'Label' instruction to avoid manually dealing with jump offsets.
-}
module JVM.Data.Abstract.Instruction where

import Data.Text (Text)
import JVM.Data.Abstract.Builder.Label (Label)
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Type (ClassInfoType, FieldType)
import JVM.Data.Raw.Types

type Reference = Int

type Instruction = Instruction' Label
data Instruction' label
    = ALoad0
    | ALoad1
    | ALoad2
    | ALoad3
    | ALoad U1
    | AStore0
    | AStore1
    | AStore2
    | AStore3
    | AStore U1
    | AReturn
    | AThrow
    | AConstNull
    | IfEq label
    | IfNe label
    | IfLt label
    | IfGe label
    | IfGt label
    | IfLe label
    | InvokeStatic ClassInfoType Text MethodDescriptor
    | InvokeInterface ClassInfoType Text MethodDescriptor
    | InvokeVirtual ClassInfoType Text MethodDescriptor
    | InvokeDynamic BootstrapMethod Text MethodDescriptor
    | Label label
    | LDC LDCEntry
    | PutStatic ClassInfoType Text FieldType
    | GetStatic ClassInfoType Text FieldType
    | Goto label
    | CheckCast ClassInfoType
    | Return
    deriving (Show, Eq, Ord, Functor)

data LDCEntry
    = LDCInt Int
    | LDCFloat Float
    | LDCString Text
    | LDCClass ClassInfoType
    deriving (Show, Eq, Ord)
