-- | High level representation of a field in a Java class file.
module H2JVM.ClassFile.Field (
    ClassFileField (..),
    FieldAttribute (..),
    ConstantValue (..),
)
where

import Data.Text (Text)

import H2JVM.ClassFile.AccessFlags (FieldAccessFlag)
import H2JVM.Internal.Pretty (Pretty (pretty), hsep, (<+>))
import H2JVM.Internal.Raw.Types (JVMDouble, JVMFloat, JVMInt, JVMLong, JVMString)
import H2JVM.Type (FieldType)

-- | A field in a Java class file.
data ClassFileField = ClassFileField
    { fieldAccessFlags :: [FieldAccessFlag]
    -- ^ The access flags of the field
    , fieldName :: Text
    -- ^ The name of the field
    , fieldType :: FieldType
    -- ^ The type of the field
    , fieldAttributes :: [FieldAttribute]
    -- ^ The attributes of the field
    }
    deriving (Show)

-- | An attribute of a field
data FieldAttribute
    = -- | The @ConstantValue@ (§4.7.2) attribute.
      ConstantValue ConstantValue
    | Synthetic -- The @Synthetic@ (§4.7.8) attribute
    deriving (Show)

-- | A Constant value that a field may have.
data ConstantValue
    = ConstantLong JVMLong
    | ConstantFloat JVMFloat
    | ConstantDouble JVMDouble
    | ConstantInteger JVMInt
    | ConstantString JVMString
    deriving (Show)

instance Pretty ClassFileField where
    pretty (ClassFileField accessFlags name fieldType attributes) =
        hsep (pretty <$> accessFlags) <+> pretty fieldType <> " " <> pretty name <> " " <> pretty attributes

instance Pretty FieldAttribute where
    pretty (ConstantValue v) = "ConstantValue " <> pretty v
    pretty Synthetic = "Synthetic"

instance Pretty ConstantValue where
    pretty (ConstantLong v) = "ConstantLong " <> pretty v
    pretty (ConstantFloat v) = "ConstantFloat " <> pretty v
    pretty (ConstantDouble v) = "ConstantDouble " <> pretty v
    pretty (ConstantInteger v) = "ConstantInteger " <> pretty v
    pretty (ConstantString v) = "ConstantString " <> pretty v
