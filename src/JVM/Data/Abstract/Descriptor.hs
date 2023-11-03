module JVM.Data.Abstract.Descriptor where

import Data.Data
import JVM.Data.Abstract.Type (FieldType)

data MethodDescriptor
    = MethodDescriptor [FieldType] ReturnDescriptor
    deriving (Show, Eq, Ord, Data)

data ReturnDescriptor
    = VoidReturn
    | TypeReturn FieldType
    deriving (Show, Eq, Ord, Data)
