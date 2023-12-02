module Analyse where

import Control.Monad (guard)
import Control.Monad.IO.Class
import Data.List.NonEmpty qualified as NE
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type

import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.Instruction
import JVM.Data.Analyse.StackMap (BasicBlock (BasicBlock), splitIntoBasicBlocks)
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
        it "Can identify basic blocks properly" $ do
            let (l, _, code) = runCodeBuilder' $ do
                    label <- newLabel
                    emit $ LDC (LDCInt 0)
                    emit $ IStore 1
                    emit $ LDC (LDCInt 0)
                    emit $ IStore 2
                    emit $ ILoad 1
                    emit $ IfLe label
                    emit $ LDC (LDCInt 0)
                    emit $ IStore 3
                    emit $ Label label
                    emit $ LDC (LDCInt 0)
                    emit $ IStore 3
                    emit Return
                    pure label
            hedgehog $ do
                let x = splitIntoBasicBlocks code

                x
                    === [ BasicBlock 0 [LDC (LDCInt 0), IStore 1, LDC (LDCInt 0), IStore 2, ILoad 1, IfLe l, LDC (LDCInt 0), IStore 3]
                        , BasicBlock 1 [LDC (LDCInt 0), IStore 3, Return]    
                        ]
