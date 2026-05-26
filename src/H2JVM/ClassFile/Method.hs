-- | High level representation of methods in a class file, including their attributes and code.
module H2JVM.ClassFile.Method (
    CodeAttribute (..),
    StackMapFrame (..),
    VerificationTypeInfo (..),
    LineNumberTableEntry (..),
    ClassFileMethod (..),
    MethodAttribute (..),
    ExceptionTableEntry (..),
    CodeAttributeData (..),
)
where

import Data.Data
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)

import H2JVM.Builder.Label
import H2JVM.ClassFile.AccessFlags (MethodAccessFlag)
import H2JVM.Data.TypeMergingList (DataMergeable (merge), TypeMergingList, errorDifferentConstructors)
import H2JVM.Descriptor (MethodDescriptor)
import H2JVM.Instruction
import H2JVM.Internal.Pretty (Pretty (pretty))
import H2JVM.Internal.Raw.Types (U1, U2)
import H2JVM.Type (ClassInfoType)

-- | A high level representation of a method in a class file.
data ClassFileMethod = ClassFileMethod
    { methodAccessFlags :: [MethodAccessFlag]
    -- ^ Access flags for the method.
    , methodName :: Text
    -- ^ The name of the method.
    , methodDescriptor :: MethodDescriptor
    -- ^ The descriptor of the method.
    , methodAttributes :: TypeMergingList MethodAttribute
    -- ^ The attributes of the method.
    }
    deriving (Show)

-- | An attribute of a method.
newtype MethodAttribute
    = -- | The bytecode instructions and execution context of the method body.
      Code CodeAttributeData
    deriving (Data, Generic, Show)

-- | The data contained in a @Code@ attribute.
data CodeAttributeData = CodeAttributeData
    { maxStack :: U2
    -- ^ The maximum stack size used by the method.
    , maxLocals :: U2
    -- ^ The maximum number of local variables used by the method.
    , code :: NonEmpty Instruction
    -- ^ The bytecode instructions of the method.
    , exceptionTable :: [ExceptionTableEntry]
    -- ^ The exception table of the method.
    , codeAttributes :: [CodeAttribute]
    -- ^ Code attributes (like stack map tables and line number tables).
    }
    deriving (Data, Generic, Show)

-- | An entry in the method's exception handler table.
data ExceptionTableEntry = ExceptionTableEntry
    { startPc :: Int
    -- ^ The start of the instruction range protected by this handler.
    , endPc :: Int
    -- ^ The end of the instruction range protected by this handler.
    , handlerPc :: Int
    -- ^ The instruction address to branch to when an exception is caught.
    , catchType :: Maybe ClassInfoType
    -- ^ The class type of the exception caught (or 'Nothing' for all/@finally@).
    }
    deriving (Data, Generic, Show)

-- | Additional properties of the compiled method bytecode.
data CodeAttribute
    = -- | Line number information for debugging stack traces.
      LineNumberTable [LineNumberTableEntry]
    | -- | Stack map frame table for JVM verification.
      StackMapTable [StackMapFrame]
    deriving (Data, Eq, Generic, Show)

instance DataMergeable CodeAttribute where
    merge (LineNumberTable a) (LineNumberTable b) = LineNumberTable (a <> b)
    merge (StackMapTable a) (StackMapTable b) = StackMapTable (a <> b)
    merge x y = errorDifferentConstructors x y

instance DataMergeable MethodAttribute where
    merge (Code a) (Code b) = Code (merge a b)

instance DataMergeable CodeAttributeData where
    merge (CodeAttributeData a b c d e) (CodeAttributeData a' b' c' d' e') =
        CodeAttributeData (max a a') (max b b') (c <> c') (d <> d') (e <> e')

-- | Represents a JVM stack map frame table entry.
data StackMapFrame
    = -- | The frame has the same local variables and an empty operand stack.
      SameFrame Label
    | -- | The operand stack is empty, and the last @k@ local variables are absent.
      ChopFrame
        -- | How many locals to chop (@k@)
        !U1
        -- | The label of the next instruction
        !Label
    | -- | The frame has the same local variables and exactly one operand stack item.
      SameLocals1StackItemFrame !VerificationTypeInfo Label
    | -- | The operand stack is empty, and new local variables are appended.
      AppendFrame ![VerificationTypeInfo] !Label
    | -- | Complete frame specification describing both locals and operand stack.
      FullFrame ![VerificationTypeInfo] ![VerificationTypeInfo] !Label
    deriving (Data, Eq, Show)

-- | Verification type metadata for a stack or local variable slot.
data VerificationTypeInfo
    = -- | Unusable or uninitialised slot.
      TopVariableInfo
    | -- | An 32-bit integer or smaller type (boolean, byte, char, short).
      IntegerVariableInfo
    | -- | A 32-bit single-precision floating point.
      FloatVariableInfo
    | -- | A 64-bit long integer.
      LongVariableInfo
    | -- | A 64-bit double-precision floating point.
      DoubleVariableInfo
    | -- | The null reference.
      NullVariableInfo
    | -- | An uninitialised reference of the current class constructor context.
      UninitializedThisVariableInfo
    | -- | A reference to an object of the specified class.
      ObjectVariableInfo !ClassInfoType
    | -- | An uninitialised reference to an object allocated by a `new` instruction at the given label.
      UninitializedVariableInfo !Label
    deriving (Data, Eq, Show)

-- | Map a JVM bytecode offset to a source code line number.
data LineNumberTableEntry = LineNumberTableEntry
    { lineNumberTableEntryStartPc :: U2
    -- ^ The starting instruction address/offset.
    , lineNumberTableEntryLineNumber :: U2
    -- ^ The source code line number.
    }
    deriving (Data, Eq, Show)

instance Pretty ClassFileMethod where
    pretty (ClassFileMethod accessFlags name descriptor attributes) =
        pretty accessFlags <> " " <> pretty descriptor <> " " <> pretty name <> " " <> pretty attributes

instance Pretty MethodAttribute where
    pretty (Code a) = "Code " <> pretty a

instance Pretty CodeAttributeData where
    pretty CodeAttributeData{maxStack, maxLocals, code, exceptionTable, codeAttributes} =
        "CodeAttributeData " <> pretty maxStack <> " " <> pretty maxLocals <> " " <> pretty code <> " " <> pretty exceptionTable <> " " <> pretty codeAttributes

instance Pretty ExceptionTableEntry where
    pretty ExceptionTableEntry{startPc, endPc, handlerPc, catchType} =
        "ExceptionTableEntry " <> pretty startPc <> " " <> pretty endPc <> " " <> pretty handlerPc <> " " <> pretty catchType

instance Pretty CodeAttribute where
    pretty (LineNumberTable a) = "LineNumberTable " <> pretty a
    pretty (StackMapTable a) = "StackMapTable " <> pretty a

instance Pretty StackMapFrame where
    pretty (SameFrame a) = "SameFrame " <> pretty a
    pretty (ChopFrame a b) = "ChopFrame " <> pretty a <> " " <> pretty b
    pretty (SameLocals1StackItemFrame a b) = "SameLocals1StackItemFrame " <> pretty a <> " " <> pretty b
    pretty (AppendFrame a b) = "AppendFrame " <> pretty a <> " " <> pretty b
    pretty (FullFrame a b c) = "FullFrame " <> pretty a <> " " <> pretty b <> " " <> pretty c

instance Pretty VerificationTypeInfo where
    pretty TopVariableInfo = "TopVariableInfo"
    pretty IntegerVariableInfo = "IntegerVariableInfo"
    pretty FloatVariableInfo = "FloatVariableInfo"
    pretty LongVariableInfo = "LongVariableInfo"
    pretty DoubleVariableInfo = "DoubleVariableInfo"
    pretty NullVariableInfo = "NullVariableInfo"
    pretty UninitializedThisVariableInfo = "UninitializedThisVariableInfo"
    pretty (ObjectVariableInfo a) = "ObjectVariableInfo " <> pretty a
    pretty (UninitializedVariableInfo a) = "UninitializedVariableInfo " <> pretty a

instance Pretty LineNumberTableEntry where
    pretty LineNumberTableEntry{lineNumberTableEntryStartPc, lineNumberTableEntryLineNumber} =
        "LineNumberTableEntry " <> pretty lineNumberTableEntryStartPc <> " " <> pretty lineNumberTableEntryLineNumber
