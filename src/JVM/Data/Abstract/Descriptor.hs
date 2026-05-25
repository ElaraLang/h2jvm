module JVM.Data.Abstract.Descriptor (MethodDescriptor (..), ReturnDescriptor (..), returnDescriptorType, methodParam) where

import Data.Data

import JVM.Data.Abstract.Type (FieldType)
import JVM.Data.Pretty

data MethodDescriptor = MethodDescriptor
    { params :: [FieldType]
    , returnDesc :: ReturnDescriptor
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

data ReturnDescriptor
    = VoidReturn
    | TypeReturn FieldType
    deriving (Data, Eq, Ord, Show)

instance Pretty ReturnDescriptor where
    pretty VoidReturn = "V"
    pretty (TypeReturn t) = pretty t

returnDescriptorType :: ReturnDescriptor -> Maybe FieldType
returnDescriptorType VoidReturn = Nothing
returnDescriptorType (TypeReturn t) = Just t
