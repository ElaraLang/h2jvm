{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

{- | High level representation of a JVM instruction, with type-safe arguments and no stack manipulation needed.
This is not a 1-1 mapping to the actual instructions, use "H2JVM.Internal.Raw.Instruction" for that.
Instead, this includes some conveniences like the 'Label' instruction to avoid manually dealing with jump offsets.
-}
module H2JVM.Instruction (
    Instruction,
    Instruction' (..),
    LDCEntry (..),
    IfCond (..),
    jumpTarget,
    ldcEntryToFieldType,
)
where

import Data.Data (Data)
import Data.Text (Text)
import GHC.Generics (Generic)

import H2JVM.Builder.Label (Label)
import H2JVM.ConstantPool
import H2JVM.Descriptor
import H2JVM.Internal.Pretty
import H2JVM.Internal.Raw.Instruction ()
import H2JVM.Internal.Raw.Types
import H2JVM.Type

{- | An instruction whose labels are the 'Label' type.
This is probably the type you want.
-}
type Instruction = Instruction' Label

-- | An instruction with a polymorphic label type.
data Instruction' label
    = -- | @aload_<n>@
      ALoad U2
    | -- | @astore_<n>@
      AStore U2
    | AReturn
    | AConstNull
    | Dup
    | IAnd
    | -- | Encodes @if_<cond>@
      If (IfCond label)
    | Instanceof ClassInfoType
    | InvokeStatic ClassInfoType Text MethodDescriptor
    | InvokeInterface ClassInfoType Text MethodDescriptor
    | InvokeVirtual ClassInfoType Text MethodDescriptor
    | InvokeDynamic BootstrapMethod Text MethodDescriptor
    | InvokeSpecial ClassInfoType Text MethodDescriptor
    | IOr
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
    | IReturn
    | IConst0
    | IConst1
    | New ClassInfoType
    | ArrayLength
    | AALoad
    | -- | Encodes @if_icmp<cond>@
      IfICmp (IfCond label)
    | IAdd
    | ISub
    | IMul
    | IDiv
    deriving (Data, Eq, Functor, Generic, Ord, Show)

-- | A condition of an if instruction.
data IfCond label = IfEq label | IfNe label | IfLt label | IfGe label | IfGt label | IfLe label
    deriving (Data, Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Pretty label => Pretty (IfCond label) where
    pretty (IfEq l) = "ifeq" <+> pretty l
    pretty (IfNe l) = "ifne" <+> pretty l
    pretty (IfLt l) = "iflt" <+> pretty l
    pretty (IfGe l) = "ifge" <+> pretty l
    pretty (IfGt l) = "ifgt" <+> pretty l
    pretty (IfLe l) = "ifle" <+> pretty l

instance Pretty label => Pretty (Instruction' label) where
    pretty AALoad = "aaload"
    pretty ArrayLength = "arraylength"
    pretty (ALoad x) = "aload" <+> pretty x
    pretty (AStore x) = "astore" <+> pretty x
    pretty AReturn = "areturn"
    pretty AConstNull = "aconst_null"
    pretty Dup = "dup"
    pretty IAnd = "iand"
    pretty (If cond) = "if" <> pretty cond
    pretty (Instanceof c) = "instanceof" <+> pretty c
    pretty (InvokeStatic c n d) = "invokestatic" <+> pretty c <> "." <> pretty n <> pretty d
    pretty (InvokeInterface c n d) = "invokeinterface" <+> pretty c <> "." <> pretty n <> pretty d
    pretty (InvokeVirtual c n d) = "invokevirtual" <+> pretty c <> "." <> pretty n <> pretty d
    pretty (InvokeDynamic b n d) = "invokedynamic" <+> pretty b <> "." <> pretty n <> pretty d
    pretty (InvokeSpecial c n d) = "invokespecial" <+> pretty c <> "." <> pretty n <> pretty d
    pretty IOr = "ior"
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
    pretty IReturn = "ireturn"
    pretty IConst0 = "iconst_0"
    pretty IConst1 = "iconst_1"
    pretty (New c) = "new" <+> pretty c
    pretty (IfICmp cmp) = "if_icmp" <> pretty cmp
    pretty IAdd = "iadd"
    pretty ISub = "isub"
    pretty IMul = "imul"
    pretty IDiv = "idiv"

-- | The jump target of an instruction, if it is a jump instruction.
jumpTarget :: Instruction' label -> Maybe label
jumpTarget (If cond) = Just $ condJumpTarget cond
jumpTarget (Goto l) = Just l
jumpTarget (IfICmp cond) = Just $ condJumpTarget cond
jumpTarget _ = Nothing

condJumpTarget :: IfCond label -> label
condJumpTarget = \case
    IfEq l -> l
    IfNe l -> l
    IfLt l -> l
    IfGe l -> l
    IfGt l -> l
    IfLe l -> l

-- | A value that can be loaded by the 'LDC' instruction.
data LDCEntry
    = -- | Load an integer constant.
      LDCInt JVMInt
    | -- | Load a float constant.
      LDCFloat Float
    | -- | Load a string constant.
      LDCString Text
    | -- | Load a Class object.
      LDCClass ClassInfoType
    deriving (Data, Eq, Generic, Ord, Show)

instance Pretty LDCEntry where
    pretty (LDCInt x) = pretty x
    pretty (LDCFloat x) = pretty x
    pretty (LDCString x) = pretty x
    pretty (LDCClass x) = pretty x

-- | Convert an 'LDCEntry to a 'FieldType.
ldcEntryToFieldType :: LDCEntry -> FieldType
ldcEntryToFieldType (LDCInt _) = PrimitiveFieldType JInt
ldcEntryToFieldType (LDCFloat _) = PrimitiveFieldType JFloat
ldcEntryToFieldType (LDCString _) = ObjectFieldType "java/lang/String"
ldcEntryToFieldType (LDCClass (ClassInfoType x)) = ObjectFieldType x
ldcEntryToFieldType (LDCClass (ArrayClassInfoType x)) = ArrayFieldType $ ldcEntryToFieldType $ LDCClass x
ldcEntryToFieldType (LDCClass (PrimitiveClassInfoType x)) = PrimitiveFieldType x
