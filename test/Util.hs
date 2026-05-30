module Util where

import Data.List.NonEmpty (NonEmpty)
import Effectful
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog
import Hedgehog.Internal.Property

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import H2JVM.Analyse.StackMap
import H2JVM.Builder.Code
import H2JVM.Internal.Convert.ConstantPool (ConstantPoolState)
import H2JVM.Internal.Convert.Instruction (convertInstructions, fullyRunCodeConverter)
import H2JVM.Internal.Convert.Monad (runConvertM)
import H2JVM.Name
import H2JVM.Type

import H2JVM.Instruction qualified as Abs
import H2JVM.Internal.Raw.Instruction qualified as Raw

genPrimitiveType :: Gen PrimitiveType
genPrimitiveType =
    Gen.element @[]
        [minBound .. maxBound]

genQualifiedClassName :: Gen QualifiedClassName
genQualifiedClassName = do
    package <- Gen.list (Range.linear 0 10) (Gen.text (Range.linear 0 10) Gen.alphaNum)
    class_ <- Gen.text (Range.linear 0 10) Gen.alphaNum
    pure $ QualifiedClassName (PackageName package) (parseClassName class_)

runAnalysis :: Eff '[CodeBuilder] a -> (NonEmpty BasicBlock, a)
runAnalysis builder =
    let (val, _, code) = runPureEff $ runCodeBuilder builder
     in (splitIntoBasicBlocks code, val)

runConv :: MonadTest m => NonEmpty Abs.Instruction -> m (NonEmpty Raw.Instruction, ConstantPoolState)
runConv =
    withFrozenCallStack $
        evalEither
            . runPureEff
            . runConvertM
            . fullyRunCodeConverter
            . convertInstructions

shouldBeJust :: MonadTest m => HasCallStack => Maybe a -> m a
shouldBeJust (Just a) = pure a
shouldBeJust Nothing = withFrozenCallStack $ failWith Nothing "Expected Just, got Nothing" -- This is safe because we know that the expectationFailure function will never return

shouldContain :: (HasCallStack, MonadTest m, Eq a, Show a, Foldable t, Show (t a)) => t a -> a -> m ()
shouldContain t a = withFrozenCallStack $ diff a elem t
