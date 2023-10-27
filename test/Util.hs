module Util where

import JVM.Data.Abstract.Instruction qualified as Abs
import JVM.Data.Convert.ConstantPool (ConstantPoolState)
import JVM.Data.Convert.Instruction (convertInstructions, fullyRunCodeConverter)
import JVM.Data.Convert.Monad (runConvertM)
import JVM.Data.Raw.Instruction qualified as Raw
import Test.Hspec (HasCallStack, expectationFailure)

runConv :: [Abs.Instruction] -> IO ([Raw.Instruction], ConstantPoolState)
runConv =
    shouldBeRight
        . runConvertM
        . fullyRunCodeConverter
        . convertInstructions

shouldBeJust :: HasCallStack => Maybe a -> IO a
shouldBeJust (Just a) = pure a
shouldBeJust Nothing = undefined <$ expectationFailure "Expected Just, got Nothing" -- This is safe because we know that the expectationFailure function will never return

shouldBeRight :: (HasCallStack, Show a) => Either a b -> IO b
shouldBeRight (Right b) = pure b
shouldBeRight (Left a) = undefined <$ expectationFailure ("Expected Right, got Left " ++ show a)

shouldContain :: (HasCallStack, Eq a, Show a, Foldable t, Show (t a)) => t a -> a -> IO ()
shouldContain t a = if a `elem` t then pure () else expectationFailure ("Expected " ++ show a ++ " to be in " ++ show t)
