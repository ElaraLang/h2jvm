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
import JVM.Data.Abstract.Type
import JVM.Data.Raw.Types

type Reference = Int

type Instruction = Instruction' Label
data Instruction' label
    = ALoad U1
    | AStore U1
    | AReturn
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
    | GetField ClassInfoType Text FieldType
    | GetStatic ClassInfoType Text FieldType
    | Goto label
    | CheckCast ClassInfoType
    | Return
    deriving (Show, Eq, Ord, Functor)

jumpTarget :: Instruction' label -> Maybe label
jumpTarget (IfEq l) = Just l
jumpTarget (IfNe l) = Just l
jumpTarget (IfLt l) = Just l
jumpTarget (IfGe l) = Just l
jumpTarget (IfGt l) = Just l
jumpTarget (IfLe l) = Just l
jumpTarget (Goto l) = Just l
jumpTarget _ = Nothing


data LDCEntry
    = LDCInt Int
    | LDCFloat Float
    | LDCString Text
    | LDCClass ClassInfoType
    deriving (Show, Eq, Ord)

ldcEntryToFieldType :: LDCEntry -> FieldType
ldcEntryToFieldType (LDCInt _) = PrimitiveFieldType Int
ldcEntryToFieldType (LDCFloat _) = PrimitiveFieldType Float
ldcEntryToFieldType (LDCString _) = ObjectFieldType "java/lang/String"
ldcEntryToFieldType (LDCClass (ClassInfoType x)) = ObjectFieldType x
ldcEntryToFieldType (LDCClass (ArrayClassInfoType x)) = ArrayFieldType $ ldcEntryToFieldType $ LDCClass x
ldcEntryToFieldType (LDCClass (PrimitiveClassInfoType x)) = PrimitiveFieldType x