module Analyse where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import JVM.Data.Abstract.Type

import Effectful
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.Method (StackMapFrame (..), VerificationTypeInfo (..))
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Analyse.StackMap (BasicBlock (BasicBlock), Frame (..), LocalVariable (..), analyseBlockDiff, calculateStackMapFrames, diffFrames, splitIntoBasicBlocks, topFrame)
import JVM.Data.Pretty
import Test.Hspec
import Test.Hspec.Hedgehog
import Util

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
    elaraTestCase1
    describe "Does StackDiff concatenation correctly" $ do
        it "Can identify incredibly simple blocks properly" $ do
            let (_, _, code) = runPureEff $ runCodeBuilder $ do
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

                diffFrames top nextFrame l
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

                diffFrames top nextFrame l
                    === AppendFrame [IntegerVariableInfo, IntegerVariableInfo] l

elaraTestCase1 :: SpecWith ()
elaraTestCase1 = describe "Elara Test Case 1" $ do
    {-
    [ :label_0
    , aload 0
    , invokestatic String.stringToList(java/lang/String)Elara.Prim.List
    , astore 1
    , aload 1
    , astore 2
    , :label_1
    , aload 2
    , instanceof Elara.Prim.Cons
    , ifeq label_3
    , goto label_2
    , :label_2
    , aload 2
    , getfield Elara.Prim.List.f0 java/lang/Character
    , astore 3
    , aload 2
    , getfield Elara.Prim.List.f1 Elara.Prim.List
    , astore 4
    , aload 3
    , astore 5
    , aload 4
    , astore 6
    , new Elara.Prim.Tuple2
    , dup
    , aload 5
    , invokespecial Elara.Prim.Tuple2.<init>(java/lang/Object)V
    , astore 7
    , aload 6
    , invokestatic String.stringFromList(Elara.Prim.List)java/lang/String
    , astore 8
    , aload 7
    , aload 8
    , invokeinterface Elara/Func.run(java.lang.Object)java.lang.Object
    , checkcast Elara.Prim.Tuple2
    , astore 9
    , new Option.Some
    , dup
    , aload 9
    , invokespecial Option.Some.<init>(java/lang/Object)V
    , astore 10
    , goto label_4
    , :label_3
    , aload 2
    , instanceof Elara.Prim.Nil
    , ifeq label_6
    , goto label_5
    , :label_6
    , invokestatic Elara.Error.patternMatchFail()java/lang/Object
    , areturn
    , :label_5
    , invokestatic Option.None()Option.Option
    , astore 10
    , goto label_4
    , :label_4
    , aload 10
    , areturn ]
        -}

    it "Analyzes Elara Test Case 1 correctly" $ do
        let ((label1, label2, label3, label4), codeAttrs, code) = runPureEff $ runCodeBuilder $ do
                label0 <- newLabel
                label1 <- newLabel
                label2 <- newLabel
                label3 <- newLabel
                label4 <- newLabel
                label5 <- newLabel
                label6 <- newLabel

                emit $ Label label0
                emit $ ALoad 0
                emit $ InvokeStatic (ClassInfoType "Elara.String") "stringToList" (MethodDescriptor [ObjectFieldType "java/lang/String"] (TypeReturn (ObjectFieldType "Elara.Prim.List")))
                emit $ AStore 1
                emit $ ALoad 1
                emit $ AStore 2
                emit $ Label label1
                emit $ ALoad 2
                emit $ Instanceof (ClassInfoType "Elara.Prim.Cons")
                emit $ IfEq label3
                emit $ Goto label2
                emit $ Label label2
                emit $ ALoad 2
                emit $ GetField (ClassInfoType "Elara.Prim.List") "f0" (PrimitiveFieldType Char)
                emit $ AStore 3
                emit $ ALoad 2
                emit $ GetField (ClassInfoType "Elara.Prim.List") "f1" (ObjectFieldType "Elara.Prim.List")
                emit $ AStore 4
                emit $ ALoad 3
                emit $ AStore 5
                emit $ ALoad 4
                emit $ AStore 6
                emit $ New (ClassInfoType "Elara.Prim.Tuple2")
                emit Dup
                emit $ ALoad 5
                emit $ InvokeSpecial (ClassInfoType "Elara.Prim.Tuple2") "<init>" (MethodDescriptor [ObjectFieldType "java/lang/Object"] VoidReturn)
                emit $ AStore 7
                emit $ ALoad 6
                emit $ InvokeStatic (ClassInfoType "Elara.String") "stringFromList" (MethodDescriptor [ObjectFieldType "Elara.Prim.List"] (TypeReturn (ObjectFieldType "java/lang/String")))
                emit $ AStore 8
                emit $ ALoad 7
                emit $ ALoad 8
                emit $ InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
                emit $ CheckCast (ClassInfoType "Elara.Prim.Tuple2")
                emit $ AStore 9
                emit $ New (ClassInfoType "Option.Some")
                emit Dup
                emit $ ALoad 9
                emit $ InvokeSpecial (ClassInfoType "Option.Some") "<init>" (MethodDescriptor [ObjectFieldType "java/lang/Object"] VoidReturn)
                emit $ AStore 10
                emit $ Goto label4
                emit $ Label label3
                emit $ ALoad 2
                emit $ Instanceof (ClassInfoType "Elara.Prim.Nil")
                emit $ IfEq label6
                emit $ Goto label5
                emit $ Label label6
                emit $ InvokeStatic (ClassInfoType "Elara.Error") "patternMatchFail" (MethodDescriptor [] (TypeReturn (ObjectFieldType "java/lang/Object")))
                emit AReturn
                emit $ Label label5
                emit $ InvokeStatic (ClassInfoType "Option") "None" (MethodDescriptor [] (TypeReturn (ObjectFieldType "Option.Option")))
                emit $ AStore 10
                emit $ Goto label4
                emit $ Label label4
                emit $ ALoad 10
                emit AReturn

                pure (label1, label2, label3, label4)

        let methodDesc = MethodDescriptor [ObjectFieldType "java/lang/String", ObjectFieldType "Elara.Func"] (TypeReturn (ObjectFieldType "Option.Option"))
        let stackMap = calculateStackMapFrames methodDesc code
        liftIO $ putStrLn $ showPretty stackMap
        hedgehog $ do
            (insts, cpState) <- runConv code
            insts /== []