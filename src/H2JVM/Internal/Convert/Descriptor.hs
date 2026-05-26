module H2JVM.Internal.Convert.Descriptor (convertMethodDescriptor) where

import Data.Text (Text)

import H2JVM.Descriptor (MethodDescriptor (..), ReturnDescriptor (..))
import H2JVM.Internal.Convert.Type (fieldTypeDescriptor)

convertMethodDescriptor :: MethodDescriptor -> Text
convertMethodDescriptor (MethodDescriptor params ret) =
    let params' = map fieldTypeDescriptor params
        ret' = case ret of
            VoidReturn -> "V"
            TypeReturn t -> fieldTypeDescriptor t
     in "(" <> mconcat params' <> ")" <> ret'
