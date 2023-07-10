module JVM.Data.Abstract.Descriptor where

import JVM.Data.Abstract.Type (FieldType)

data MethodDescriptor
  = MethodDescriptor [FieldType] ReturnDescriptor
  deriving (Show, Eq, Ord)

data ReturnDescriptor
  = VoidReturn
  | TypeReturn FieldType
  deriving (Show, Eq, Ord)
