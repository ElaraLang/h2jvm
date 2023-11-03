module JVM.Data.Abstract.Descriptor where

import JVM.Data.Abstract.Type (FieldType)
import Data.Data

data MethodDescriptor
    = MethodDescriptor [FieldType] ReturnDescriptor
    deriving (Show, Eq, Ord, Data)

data ReturnDescriptor
    = VoidReturn
    | TypeReturn FieldType
    deriving (Show, Eq, Ord, Data)
