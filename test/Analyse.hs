module Analyse where

import Control.Monad (guard)
import Control.Monad.IO.Class
import Data.List.NonEmpty qualified as NE
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type
import JVM.Data.Analyse.Instruction
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

genStackDiff :: Gen StackDiff
genStackDiff =
    Gen.choice
        [ stackPush <$> Gen.list (Range.linear 0 100) genFieldType
        , stackPop <$> Gen.integral (Range.linear 0 100)
        , stackPopAndPush <$> Gen.integral (Range.linear 0 100) <*> Gen.list (Range.linear 0 100) genFieldType
        , pure StackSame
        ]

spec :: Spec
spec = describe "Analysis checks" $ do
    describe "Does StackDiff concatenation correctly" $ do
        it "Is StackSame identity" $ hedgehog $ do
            a <- forAll genStackDiff
            a <> StackSame === a
            StackSame <> a === a

        it "is stackPopAndPush valid when normalised" $ hedgehog $ do
            n <- forAll $ Gen.integral (Range.linear 1 100)
            ts <- forAll $ Gen.list (Range.linear 1 100) genFieldType

            normaliseStackDiff (stackPopAndPush n ts)
                === if n <= length ts
                    then stackPush (drop n ts)
                    else stackPop (n - length ts)

            normaliseStackDiff (stackPopAndPush n ts) === stackPop n <> stackPush ts
            stackPop n <> stackPush ts === normaliseStackDiff (stackPopAndPush n ts)

        it "is StackPush + StackPop valid" $ hedgehog $ do
            n <- forAll $ Gen.integral (Range.linear 1 100)
            ts <- forAll $ Gen.nonEmpty (Range.linear 1 100) genFieldType

            StackPop n <> StackPush ts === case n `compare` length ts of 
                LT -> stackPush (NE.drop n ts)
                EQ -> StackSame
                GT -> stackPop (n - length ts)
            StackPush ts
                <> StackPop n
                === if n <= length ts
                    then stackPush (NE.drop n ts)
                    else stackPop (n - length ts)

        it "Is StackPop + StackPopAndPush valid" $ hedgehog $ do
            n <- forAll $ Gen.integral (Range.linear 1 100)
            n2 <- forAll $ Gen.integral (Range.linear 1 100)
            ts <- forAll $ Gen.list (Range.linear 1 100) genFieldType

            stackPop n <> stackPopAndPush n2 ts === stackPopAndPush (n2 + n) ts
            stackPopAndPush n2 ts
                <> stackPop n
                === if (n + n2) <= length ts
                    then stackPush (drop (n + n2) ts)
                    else stackPop (n + n2 - length ts)

        it "Is StackPush + StackPopAndPush valid" $ hedgehog $ do
            n <- forAll $ Gen.integral (Range.linear 1 100)
            ts1 <- forAll $ Gen.list (Range.linear 1 100) genFieldType
            ts2 <- forAll $ Gen.list (Range.linear 1 100) genFieldType

            stackPopAndPush n ts1
                <> stackPush ts2
                === if n >= length ts1
                    then stackPop (n - length ts1) <> stackPush (ts2)
                    else normaliseStackDiff $ stackPopAndPush n (ts1 <> ts2)
            (stackPush ts1 <> stackPopAndPush n ts2)
                === if n <= length ts1
                    then stackPush (drop n ts1) <> stackPush ts2
                    else stackPop (n - length ts1) <> stackPush ts2
