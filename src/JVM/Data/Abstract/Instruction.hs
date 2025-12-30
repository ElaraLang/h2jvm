{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{- | High level representation of a JVM instruction, with type-safe arguments and no stack manipulation needed.
 This is not a 1-1 mapping to the actual instructions, use 'JVM.Data.Raw.Instruction' for that.
 Instead, this includes some conveniences like the 'Label' instruction to avoid manually dealing with jump offsets.
-}
module JVM.Data.Abstract.Instruction where

import Data.Data (Data)
import Data.Text (Text)
import GHC.Generics (Generic)
import JVM.Data.Abstract.Builder.Label (Label)
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Type
import JVM.Data.Pretty
import JVM.Data.Raw.Types

type Reference = Int

type Instruction = Instruction' Label
data Instruction' label
    = ALoad U2
    | AStore U2
    | AReturn
    | AConstNull
    | Dup
    | IfEq label
    | IfNe label
    | IfLt label
    | IfGe label
    | IfGt label
    | IfLe label
    | Instanceof ClassInfoType
    | InvokeStatic ClassInfoType Text MethodDescriptor
    | InvokeInterface ClassInfoType Text MethodDescriptor
    | InvokeVirtual ClassInfoType Text MethodDescriptor
    | InvokeDynamic BootstrapMethod Text MethodDescriptor
    | InvokeSpecial ClassInfoType Text MethodDescriptor
    | ILoad U2
    | IStore U2
    | Label label
    | LDC LDCEntry
    | PutStatic ClassInfoType Text FieldType
    | GetField ClassInfoType Text FieldType
    | GetStatic ClassInfoType Text FieldType
    | PutField ClassInfoType Text FieldType
    | Goto label
    | CheckCast ClassInfoType
    | Return
    | New ClassInfoType
    deriving (Show, Eq, Ord, Functor, Generic, Data)

instance (Pretty label) => Pretty (Instruction' label) where
    pretty (ALoad x) = "aload" <+> pretty x
    pretty (AStore x) = "astore" <+> pretty x
    pretty AReturn = "areturn"
    pretty AConstNull = "aconst_null"
    pretty Dup = "dup"
    pretty (IfEq l) = "ifeq" <+> pretty l
    pretty (IfNe l) = "ifne" <+> pretty l
    pretty (IfLt l) = "iflt" <+> pretty l
    pretty (IfGe l) = "ifge" <+> pretty l
    pretty (IfGt l) = "ifgt" <+> pretty l
    pretty (IfLe l) = "ifle" <+> pretty l
    pretty (Instanceof c) = "instanceof" <+> pretty c
    pretty (InvokeStatic c n d) = "invokestatic" <+> pretty c <> "." <> pretty n <> pretty d
    pretty (InvokeInterface c n d) = "invokeinterface" <+> pretty c <> "." <> pretty n <> pretty d
    pretty (InvokeVirtual c n d) = "invokevirtual" <+> pretty c <> "." <> pretty n <> pretty d
    pretty (InvokeDynamic b n d) = "invokedynamic" <+> pretty b <> "." <> pretty n <> pretty d
    pretty (InvokeSpecial c n d) = "invokespecial" <+> pretty c <> "." <> pretty n <> pretty d
    pretty (ILoad x) = "iload" <+> pretty x
    pretty (IStore x) = "istore" <+> pretty x
    pretty (Label l) = ":" <> pretty l
    pretty (LDC x) = "ldc" <+> pretty x
    pretty (PutStatic c n t) = "putstatic" <+> pretty c <> "." <> pretty n <+> pretty t
    pretty (GetField c n t) = "getfield" <+> pretty c <> "." <> pretty n <+> pretty t
    pretty (GetStatic c n t) = "getstatic" <+> pretty c <> "." <> pretty n <+> pretty t
    pretty (PutField c n t) = "putfield" <+> pretty c <> "." <> pretty n <+> pretty t
    pretty (Goto l) = "goto" <+> pretty l
    pretty (CheckCast c) = "checkcast" <+> pretty c
    pretty Return = "return"
    pretty (New c) = "new" <+> pretty c

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
    deriving (Show, Eq, Ord, Data, Generic)

instance Pretty LDCEntry where
    pretty (LDCInt x) = pretty x
    pretty (LDCFloat x) = pretty x
    pretty (LDCString x) = pretty x
    pretty (LDCClass x) = pretty x

ldcEntryToFieldType :: LDCEntry -> FieldType
ldcEntryToFieldType (LDCInt _) = PrimitiveFieldType Int
ldcEntryToFieldType (LDCFloat _) = PrimitiveFieldType Float
ldcEntryToFieldType (LDCString _) = ObjectFieldType "java/lang/String"
ldcEntryToFieldType (LDCClass (ClassInfoType x)) = ObjectFieldType x
ldcEntryToFieldType (LDCClass (ArrayClassInfoType x)) = ArrayFieldType $ ldcEntryToFieldType $ LDCClass x
ldcEntryToFieldType (LDCClass (PrimitiveClassInfoType x)) = PrimitiveFieldType x
