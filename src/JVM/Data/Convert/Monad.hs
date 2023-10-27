module JVM.Data.Convert.Monad where

import Control.Monad.Except
import Data.Word (Word16)
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Convert.ConstantPool (ConstantPoolState, ConstantPoolT, runConstantPoolM, runConstantPoolT)

type ConvertM = ConstantPoolT (Except CodeConverterError)

runConvertM :: ConvertM a -> Either CodeConverterError (a, ConstantPoolState)
runConvertM = runExcept . runConstantPoolT

data CodeConverterError
    = DuplicateLabel Label Word16
    | UnmarkedLabel Label
    deriving (Show)
