{-# LANGUAGE ConstraintKinds #-}

-- | General effect stack for all high level -> low level conversion operations.
module H2JVM.Internal.Convert.Monad (ConvertEff, runConvertM, CodeConverterError (..)) where

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

import H2JVM.Internal.Convert.ConstantPool
import H2JVM.Internal.Convert.Error

-- | A converter needs access to the 'ConstantPool' and the ability to throw 'CodeConverterError's.
type ConvertEff r = (State ConstantPoolState :> r, Error CodeConverterError :> r)

-- | Run a 'ConvertEff' as a pure 'State' & 'Either' effect.
runConvertM ::
    Eff (State ConstantPoolState : Error CodeConverterError : r) a ->
    Eff r (Either CodeConverterError (a, ConstantPoolState))
runConvertM = runErrorNoCallStack . runConstantPool
