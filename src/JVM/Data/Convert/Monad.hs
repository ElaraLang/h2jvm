{-# LANGUAGE ConstraintKinds #-}

module JVM.Data.Convert.Monad where

import Data.Word (Word16)
import Effectful
import Effectful.Error.Static
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Convert.ConstantPool (ConstantPool, ConstantPoolState, runConstantPool)

type ConvertEff r = (ConstantPool :> r, Error CodeConverterError :> r)

runConvertM :: Eff (ConstantPool : Error CodeConverterError : r) a -> Eff r (Either CodeConverterError (a, ConstantPoolState))
runConvertM = fmap mapLeft . runError . runConstantPool
  where
    mapLeft (Left (_, e)) = Left e
    mapLeft (Right b) = Right b

data CodeConverterError
    = DuplicateLabel Label Word16
    | UnmarkedLabel Label
    deriving (Show)
