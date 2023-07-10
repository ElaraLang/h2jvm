module JVM.Data.Convert.Descriptor where

import Data.Text (Text)
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), ReturnDescriptor (..))
import JVM.Data.Convert.Type (fieldTypeDescriptor)

convertMethodDescriptor :: MethodDescriptor -> Text
convertMethodDescriptor (MethodDescriptor params ret) =
  let params' = map fieldTypeDescriptor params
      ret' = case ret of
        VoidReturn -> "V"
        TypeReturn t -> fieldTypeDescriptor t
   in "(" <> mconcat params' <> ")" <> ret'
