{-# LANGUAGE ConstraintKinds #-}

-- | General effect stack for all high level -> low level conversion operations.
module H2JVM.Internal.Convert.Monad (ConvertEff, runConvertM, CodeConverterError (..)) where

import Data.Word (Word16)
import Effectful
import Effectful.Error.Static

import H2JVM.Builder.Label
import H2JVM.Internal.Convert.ConstantPool (ConstantPool, ConstantPoolState, runConstantPool)

-- | A converter needs access to the 'ConstantPool' and the ability to throw 'CodeConverterError's.
type ConvertEff r = (ConstantPool :> r, Error CodeConverterError :> r)

-- | Run a 'ConvertEff' as a pure 'State' & 'Either' effect.
runConvertM ::
    Eff (ConstantPool : Error CodeConverterError : r) a ->
    Eff r (Either CodeConverterError (a, ConstantPoolState))
runConvertM = fmap mapLeft . runError . runConstantPool
  where
    mapLeft (Left (_, e)) = Left e
    mapLeft (Right b) = Right b

-- | Errors that may be thrown during code conversion.
data CodeConverterError
    = -- | A 'Label' was used more than once.
      DuplicateLabel
        -- | The 'Label' in question.
        Label
        Word16 -- The offset of the second encountered instance of the 'Label'
    | -- | A 'Label' was referenced but never had its location defined.
      UnmarkedLabel
        Label
    deriving (Show)
