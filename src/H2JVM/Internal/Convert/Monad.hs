{-# LANGUAGE ConstraintKinds #-}

module H2JVM.Internal.Convert.Monad (ConvertEff, runConvertM, CodeConverterError (..)) where

import Data.Word (Word16)
import Effectful
import Effectful.Error.Static

import H2JVM.Builder.Label
import H2JVM.Internal.Convert.ConstantPool (ConstantPool, ConstantPoolState, runConstantPool)

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
