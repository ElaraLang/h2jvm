module Analyse where

import Effectful (runPureEff)
import Hedgehog (Gen, property, (===))
import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

import Data.List.NonEmpty qualified as NE
import Hedgehog.Gen qualified as Gen

import H2JVM.Analyse.StackMap (BasicBlock (BasicBlock), Frame (..), LocalVariable (..), analyseBlockDiff, diffFrames, splitIntoBasicBlocks, topFrame)
import H2JVM.Builder.Code (
    emit,
    newLabel,
    runCodeBuilder,
 )
import H2JVM.ClassFile.AccessFlags (MethodAccessFlag (..))
import H2JVM.ClassFile.Method (StackMapFrame (..), VerificationTypeInfo (..))
import H2JVM.Descriptor (
    MethodDescriptor (MethodDescriptor),
    ReturnDescriptor (TypeReturn, VoidReturn),
 )
import H2JVM.Instruction (
    Instruction' (
        ALoad,
        AReturn,
        AStore,
        ILoad,
        IStore,
        IfEq,
        IfLe,
        LDC,
        Label,
        Return
    ),
    LDCEntry (LDCInt),
 )
import H2JVM.Internal.Convert (jloName)
import H2JVM.Internal.Pretty ()
import H2JVM.Type (
    FieldType (..),
    PrimitiveType (JInt),
 )
import Util (genPrimitiveType, genQualifiedClassName)

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
            let (_, _, code) = runPureEff $ runCodeBuilder $ do
                    emit $ LDC (LDCInt 0) -- [0]
                    emit $ AStore 0
                    emit $ ALoad 0
                    emit AReturn

            property $ do
                let blocks = splitIntoBasicBlocks code

                NE.toList blocks
                    === [ BasicBlock 0 [LDC (LDCInt 0), AStore 0, ALoad 0, AReturn] Nothing Nothing
                        ]

                let top = topFrame jloName [MStatic] (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JInt)))
                let nextFrame = analyseBlockDiff top (NE.head blocks)

                nextFrame
                    === Frame
                        { locals = [LocalVariable (PrimitiveFieldType JInt)]
                        , stack = []
                        }

        it "Can identify sameframe blocks properly" $ do
            let (l, _, code) = runPureEff $ runCodeBuilder $ do
                    label <- newLabel
                    emit $ LDC (LDCInt 0) -- [0]
                    emit $ IfEq label
                    emit $ LDC (LDCInt 0)
                    emit AReturn
                    emit $ Label label
                    emit $ LDC (LDCInt 1)
                    emit AReturn

                    pure label
            property $ do
                let blocks = splitIntoBasicBlocks code

                NE.toList blocks
                    === [ BasicBlock 0 [LDC (LDCInt 0), IfEq l] Nothing Nothing
                        , BasicBlock 1 [LDC (LDCInt 0), AReturn] Nothing (Just l)
                        , BasicBlock 2 [LDC (LDCInt 1), AReturn] (Just l) Nothing
                        ]

                let top = topFrame jloName [MStatic] (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JInt)))
                let nextFrame = analyseBlockDiff top (NE.head blocks)

                nextFrame
                    === Frame
                        { locals = []
                        , stack = []
                        }

                let nextFrame' = analyseBlockDiff nextFrame (blocks NE.!! 2)

                nextFrame'
                    === Frame
                        { locals = []
                        , stack = []
                        }

                diffFrames top nextFrame' l
                    === SameFrame l

        it "Can identify append frame blocks properly" $ do
            let (l, _, code) = runPureEff $ runCodeBuilder $ do
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
            property $ do
                let blocks = splitIntoBasicBlocks code

                NE.toList blocks
                    === [ BasicBlock 0 [LDC (LDCInt 0), IStore 0, LDC (LDCInt 0), IStore 1, ILoad 0, IfLe l] Nothing Nothing
                        , BasicBlock 1 [LDC (LDCInt 0), IStore 2] Nothing (Just l)
                        , BasicBlock 2 [LDC (LDCInt 0), IStore 2, Return] (Just l) Nothing
                        ]

                let top = topFrame jloName [MStatic] (MethodDescriptor [] VoidReturn)
                let nextFrame = analyseBlockDiff top (NE.head blocks)

                nextFrame
                    === Frame
                        { locals = [LocalVariable (PrimitiveFieldType JInt), LocalVariable (PrimitiveFieldType JInt)]
                        , stack = []
                        }

                let nextFrame' = analyseBlockDiff nextFrame (blocks NE.!! 2)

                nextFrame'
                    === Frame
                        { locals = [LocalVariable (PrimitiveFieldType JInt), LocalVariable (PrimitiveFieldType JInt), LocalVariable (PrimitiveFieldType JInt)]
                        , stack = []
                        }

                -- The frame at label l should now have 2 locals when we arrive at it from block 0
                diffFrames top nextFrame l
                    === AppendFrame [IntegerVariableInfo, IntegerVariableInfo] l
