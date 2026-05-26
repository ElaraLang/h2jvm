module H2JVM.Descriptor (MethodDescriptor (..), ReturnDescriptor (..), returnDescriptorType, methodParam) where

import Data.Data

import H2JVM.Internal.Pretty
import H2JVM.Type (FieldType)

-- | Represents a JVM method type signature descriptor (§4.3.3).
data MethodDescriptor = MethodDescriptor
    { params :: [FieldType]
    -- ^ The parameter types of the method.
    , returnDesc :: ReturnDescriptor
    -- ^ The return type descriptor of the method.
    }
    deriving (Data, Eq, Ord, Show)

instance Pretty MethodDescriptor where
    pretty (MethodDescriptor params ret) = "(" <> hsep (pretty <$> params) <> ")" <> pretty ret

-- | Get the type of the i-th parameter of a method descriptor, if it exists.
methodParam :: MethodDescriptor -> Int -> Maybe FieldType
methodParam (MethodDescriptor params _) i = params !!? i
  where
    [] !!? _ = Nothing
    (x : _) !!? 0 = Just x
    (_ : xs) !!? n = xs !!? (n - 1)

-- | Represents a method return type descriptor.
data ReturnDescriptor
    = -- | A void return (@V@).
      VoidReturn
    | -- | A return of some 'FieldType'.
      TypeReturn FieldType
    deriving (Data, Eq, Ord, Show)

instance Pretty ReturnDescriptor where
    pretty VoidReturn = "V"
    pretty (TypeReturn t) = pretty t

returnDescriptorType :: ReturnDescriptor -> Maybe FieldType
returnDescriptorType VoidReturn = Nothing
returnDescriptorType (TypeReturn t) = Just t
