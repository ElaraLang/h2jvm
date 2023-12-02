module Analyse where

import Control.Monad (guard)
import Control.Monad.IO.Class
import Data.List.NonEmpty qualified as NE
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type

import Test.Hspec
import Test.Hspec.Hedgehog

genPrimitiveType :: Gen PrimitiveType
genPrimitiveType =
    Gen.element
        [ Byte
        , Char
        , Double
        , Float
        , Int
        , Long
        , Short
        , Boolean
        ]

genQualifiedClassName :: Gen QualifiedClassName
genQualifiedClassName = do
    package <- Gen.list (Range.linear 0 10) (Gen.text (Range.linear 0 10) Gen.alphaNum)
    class_ <- Gen.text (Range.linear 0 10) Gen.alphaNum
    pure $ QualifiedClassName (PackageName package) (ClassName class_)

genFieldType :: Gen FieldType
genFieldType =
    Gen.recursive
        Gen.choice
        [ PrimitiveFieldType <$> genPrimitiveType
        , ObjectFieldType <$> genQualifiedClassName
        ]
        [ Gen.subterm genFieldType ArrayFieldType
        ]



spec :: Spec
spec = describe "Analysis checks" $ do
    describe "Does StackDiff concatenation correctly" $ do
        pure ()