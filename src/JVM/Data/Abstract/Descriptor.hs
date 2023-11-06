module JVM.Data.Abstract.Descriptor where

import Data.Data
import JVM.Data.Abstract.Type (FieldType)

data MethodDescriptor
    = MethodDescriptor [FieldType] ReturnDescriptor
    deriving (Show, Eq, Ord, Data)

methodParam :: MethodDescriptor -> Int -> Maybe FieldType
methodParam (MethodDescriptor params _) i = params !!? i
  where
    [] !!? _ = Nothing
    (x : _) !!? 0 = Just x
    (_ : xs) !!? n = xs !!? (n - 1)

data ReturnDescriptor
    = VoidReturn
    | TypeReturn FieldType
    deriving (Show, Eq, Ord, Data)
