module Util where

import GHC.Stack (withFrozenCallStack)
import Hedgehog
import Hedgehog.Internal.Property
import JVM.Data.Abstract.Instruction qualified as Abs
import JVM.Data.Convert.ConstantPool (ConstantPoolState)
import JVM.Data.Convert.Instruction (convertInstructions, fullyRunCodeConverter)
import JVM.Data.Convert.Monad (runConvertM)
import JVM.Data.Raw.Instruction qualified as Raw
import Test.Hspec (HasCallStack)

runConv :: MonadTest m => [Abs.Instruction] -> m ([Raw.Instruction], ConstantPoolState)
runConv =
    withFrozenCallStack $
        shouldBeRight
            . runConvertM
            . fullyRunCodeConverter
            . convertInstructions

shouldBeJust :: MonadTest m => HasCallStack => Maybe a -> m a
shouldBeJust (Just a) = pure a
shouldBeJust Nothing = withFrozenCallStack $ failWith Nothing "Expected Just, got Nothing" -- This is safe because we know that the expectationFailure function will never return

shouldBeRight :: (MonadTest m, HasCallStack, Show a) => Either a b -> m b
shouldBeRight (Right b) = pure b
shouldBeRight (Left a) = withFrozenCallStack $ failWith Nothing ("Expected Right, got Left " ++ show a)

shouldContain :: (HasCallStack, MonadTest m, Eq a, Show a, Foldable t, Show (t a)) => t a -> a -> m ()
shouldContain t a = withFrozenCallStack $ diff a elem t
