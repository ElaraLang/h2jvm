module Util where

import Data.List.NonEmpty (NonEmpty)
import Effectful
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog
import Hedgehog.Internal.Property

import Data.List.NonEmpty qualified as NE
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type
import JVM.Data.Analyse.StackMap
import JVM.Data.Convert.ConstantPool (ConstantPoolState)
import JVM.Data.Convert.Instruction (convertInstructions, fullyRunCodeConverter)
import JVM.Data.Convert.Monad (runConvertM)

import JVM.Data.Abstract.Instruction qualified as Abs
import JVM.Data.Raw.Instruction qualified as Raw

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
        shouldBeRight
            . runPureEff
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
