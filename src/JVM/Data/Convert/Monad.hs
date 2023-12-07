{-# LANGUAGE ConstraintKinds #-}

module JVM.Data.Convert.Monad where

import Data.Word (Word16)
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Convert.ConstantPool (ConstantPool, ConstantPoolState, runConstantPool)
import Polysemy
import Polysemy.Error

type ConvertEff r = Members '[ConstantPool, Error CodeConverterError]r

runConvertM :: Sem (ConstantPool : Error CodeConverterError : r) a -> Sem r (Either CodeConverterError (a, ConstantPoolState))
runConvertM = runError . runConstantPool

data CodeConverterError
    = DuplicateLabel Label Word16
    | UnmarkedLabel Label
    deriving (Show)
