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

newtype MethodAttribute
    = Code CodeAttributeData
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
    -- ^ The attributes of the code, such as line number tables and stack map tables.
    }
    deriving (Data, Generic, Show)

-- | An entry in the exception table of a method's code attribute.
data ExceptionTableEntry = ExceptionTableEntry
    { startPc :: U2
    -- ^ The bytecode offset of the start of the protected region.
    , endPc :: U2
    -- ^ The (exclusive) bytecode offset of the end of the protected region.
    , handlerPc :: U2
    -- ^ The bytecode offset of the exception handler.
    , catchType :: Maybe ClassInfoType
    -- ^ The type of exception to catch, or 'Nothing' to catch all exceptions.
    }
    deriving (Data, Generic, Show)

-- | A code attribute.
data CodeAttribute
    = -- | The @LineNumberTable@ (§4.7.12) attribute.
      LineNumberTable [LineNumberTableEntry]
    | -- | The @StackMapTable@ (§4.7.4) attribute.
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

-- | A stack map frame, used in the @StackMapTable@ attribute to verify safety of bytecode instructions.
data StackMapFrame
    = -- | The frame has the same locals, and the stack is empty.
      SameFrame
        -- | The label of the next instruction
        Label
    | -- | the frame has the same locals, and the stack has one item. This type encodes both @same_locals_1_stack_item_frame@ and @same_locals_1_stack_item_frame_extended@.
      SameLocals1StackItemFrame
        -- | The verification type of the one stack item
        !VerificationTypeInfo
        -- | The label of the next instruction
        Label
    | -- |  The stack is empty, and the frame has the same locals as the previous frame, except that the last @n@ locals are absent.
      ChopFrame
        -- | How many locals to chop
        !U1
        -- | The label of the next instruction
        !Label
    | -- | the stack is empty, and the frame has the same locals as the previous frame, except that it has @n@ additional locals.
      AppendFrame
        -- | The verification types of the additional locals
        ![VerificationTypeInfo]
        -- | The label of the next instruction
        !Label
    | -- | A full frame, which explicitly specifies the verification types of all locals and stack items.
      FullFrame
        -- | The verification types of all local variables
        ![VerificationTypeInfo]
        -- | The verification types of all stack items
        ![VerificationTypeInfo]
        -- | The label of the next instruction
        !Label
    deriving (Data, Eq, Show)

-- | The @verification_type_info@ type, used in stack map frames to verify the types of local variables and stack items.
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
    deriving (Data, Eq, Show)

-- | An entry in the line number table of a method's code attribute.
data LineNumberTableEntry = LineNumberTableEntry
    { lineNumberTableEntryStartPc :: U2
    -- ^ the index into the code array at which the code for a new line begins
    , lineNumberTableEntryLineNumber :: U2
    -- ^ the line number in the source file corresponding to the code at the given index
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
