module Analyse where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type

import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.Method (StackMapFrame (..), VerificationTypeInfo (..))
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Analyse.StackMap (BasicBlock (BasicBlock), Frame (..), LocalVariable (..), analyseBlockDiff, frameDiffToSMF, splitIntoBasicBlocks, topFrame)
import Polysemy
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
        it "Can identify incredibly simple blocks properly" $ do
            let (_, _, code) = run $ runCodeBuilder $ do
                    emit $ LDC (LDCInt 0) -- [0]
                    emit $ AStore 0
                    emit $ ALoad 0
                    emit AReturn

            hedgehog $ do
                let blocks = splitIntoBasicBlocks code

                blocks
                    === [ BasicBlock 0 [LDC (LDCInt 0), AStore 0, ALoad 0, AReturn] Nothing
                        ]

                let top = topFrame (MethodDescriptor [] (TypeReturn (PrimitiveFieldType Int)))
                let nextFrame = analyseBlockDiff top (head blocks)

                nextFrame
                    === Frame
                        { locals = [LocalVariable (PrimitiveFieldType Int)]
                        , stack = []
                        }

        it "Can identify sameframe blocks properly" $ do
            let (l, _, code) = run $ runCodeBuilder $ do
                    label <- newLabel
                    emit $ LDC (LDCInt 0) -- [0]
                    emit $ IfEq label
                    emit $ LDC (LDCInt 0)
                    emit AReturn
                    emit $ Label label
                    emit $ LDC (LDCInt 1)
                    emit AReturn

                    pure label
            hedgehog $ do
                let blocks = splitIntoBasicBlocks code

                blocks
                    === [ BasicBlock 0 [LDC (LDCInt 0), IfEq l, LDC (LDCInt 0), AReturn] (Just l)
                        , BasicBlock 1 [LDC (LDCInt 1), AReturn] Nothing
                        ]

                let top = topFrame (MethodDescriptor [] (TypeReturn (PrimitiveFieldType Int)))
                let nextFrame = analyseBlockDiff top (head blocks)

                nextFrame
                    === Frame
                        { locals = []
                        , stack = []
                        }

                let nextFrame' = analyseBlockDiff nextFrame (blocks !! 1)

                nextFrame'
                    === Frame
                        { locals = []
                        , stack = []
                        }

                frameDiffToSMF top (head blocks)
                    === SameFrame l

        it "Can identify append frame blocks properly" $ do
            let (l, _, code) = run $ runCodeBuilder $ do
                    label <- newLabel
                    emit $ LDC (LDCInt 0) -- [0]
                    emit $ IStore 0 -- []
                    emit $ LDC (LDCInt 0) -- [0]
                    emit $ IStore 1 -- []
                    emit $ ILoad 0 -- [0]
                    emit $ IfLe label -- []
                    emit $ LDC (LDCInt 0) -- [0]
                    emit $ IStore 2 -- []
                    emit $ Label label -- []
                    emit $ LDC (LDCInt 0) -- [0]
                    emit $ IStore 2 -- []
                    emit Return -- []
                    pure label
            hedgehog $ do
                let blocks = splitIntoBasicBlocks code

                blocks
                    === [ BasicBlock 0 [LDC (LDCInt 0), IStore 0, LDC (LDCInt 0), IStore 1, ILoad 0, IfLe l, LDC (LDCInt 0), IStore 2] (Just l)
                        , BasicBlock 1 [LDC (LDCInt 0), IStore 2, Return] Nothing
                        ]

                let top = topFrame (MethodDescriptor [] VoidReturn)
                let nextFrame = analyseBlockDiff top (head blocks)

                nextFrame
                    === Frame
                        { locals = [LocalVariable (PrimitiveFieldType Int), LocalVariable (PrimitiveFieldType Int)]
                        , stack = []
                        }

                let nextFrame' = analyseBlockDiff nextFrame (blocks !! 1)

                nextFrame'
                    === Frame
                        { locals = [LocalVariable (PrimitiveFieldType Int), LocalVariable (PrimitiveFieldType Int), LocalVariable (PrimitiveFieldType Int)]
                        , stack = []
                        }

                frameDiffToSMF top (head blocks)
                    === AppendFrame [IntegerVariableInfo, IntegerVariableInfo] l
