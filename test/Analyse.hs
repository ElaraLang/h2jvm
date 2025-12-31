module Analyse where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import JVM.Data.Abstract.Type

import Effectful
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.Method (StackMapFrame (..), VerificationTypeInfo (..))
import JVM.Data.Abstract.ConstantPool (BootstrapArgument (..), BootstrapMethod (..), MethodHandleEntry (..), MethodRef (..))
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
    :label_0
    , aload 0
    , astore 1
    , :label_1
    , aload 1
    , instanceof Main.Parser
    , ifeq label_3
    , goto label_2
    , :label_3
    , invokestatic Elara.Error.patternMatchFail()java/lang/Object
    , areturn
    , :label_2
    , aload 1
    , getfield Main.Parser.f0 java/lang/Object
    , astore 2
    , aload 2
    , astore 3
    , aload 3
    , invokedynamic invokeStatic java/lang/invoke/LambdaMetafactory.metafactory(java/lang/invoke/MethodHandles$Lookup java/lang/String java/lang/invoke/MethodType java/lang/invoke/MethodType java/lang/invoke/MethodHandle java/lang/invoke/MethodType)java/lang/invoke/CallSite (java.lang.Object)java.lang.Object invokeStatic Main.lifted4913(Elara/Func java/lang/String)Option.Option (java/lang/String)Option.Option.run(Elara/Func)Elara/Func
    , astore 4
    , aload 4
    , astore 5
    , goto label_4
    , :label_4
    , aload 5
    , areturn
        -}

    it "Analyzes Elara Test Case 1 correctly" $ do
        let ((label1, label2, label3, label4), codeAttrs, code) = runPureEff $ runCodeBuilder $ do
                label0 <- newLabel
                emit $ Label label0
                emit $ ALoad 0
                emit $ AStore 1
                label1 <- newLabel
                emit $ Label label1
                emit $ ALoad 1
                emit $ Instanceof (ClassInfoType "Main/Parser")
                label3 <- newLabel
                emit $ IfEq label3
                label2 <- newLabel
                emit $ Goto label2
                emit $ Label label3
                emit $ InvokeStatic (ClassInfoType "Elara/Error") "patternMatchFail" (MethodDescriptor [] (TypeReturn (ObjectFieldType "java/lang/Object")))
                emit AReturn
                emit $ Label label2
                emit $ ALoad 1
                emit $ GetField (ClassInfoType "Main/Parser") "f0" (ObjectFieldType "java/lang/Object")
                emit $ AStore 2
                emit $ ALoad 2
                emit $ AStore 3
                emit $ ALoad 3
                emit $
                    InvokeDynamic
                        ( BootstrapMethod
                            ( MHInvokeStatic
                                ( MethodRef
                                    (ClassInfoType "java/lang/invoke/LambdaMetafactory")
                                    "metafactory"
                                    ( MethodDescriptor
                                        [ ObjectFieldType "java/lang/invoke/MethodHandles$Lookup"
                                        , ObjectFieldType "java/lang/String"
                                        , ObjectFieldType "java/lang/invoke/MethodType"
                                        , ObjectFieldType "java/lang/invoke/MethodType"
                                        , ObjectFieldType "java/lang/invoke/MethodHandle"
                                        , ObjectFieldType "java/lang/invoke/MethodType"
                                        ]
                                        (TypeReturn (ObjectFieldType "java/lang/invoke/CallSite"))
                                    )
                                )
                            )
                            [BMClassArg (ClassInfoType "java/lang/Object")]
                        )
                        "invokeStatic"
                        ( MethodDescriptor
                            [ ObjectFieldType "Elara/Func"
                            , ObjectFieldType "java/lang/String"
                            ]
                            (TypeReturn (ObjectFieldType "Option/Option"))
                        )
                label4 <- newLabel
                emit $ AStore 4
                emit $ ALoad 4
                emit $ AStore 5
                emit $ Goto label4
                emit $ Label label4
                emit $ ALoad 5
                emit AReturn

                pure (label1, label2, label3, label4)

        let methodDesc = MethodDescriptor [ObjectFieldType "java/lang/String", ObjectFieldType "Elara.Func"] (TypeReturn (ObjectFieldType "Option.Option"))
        let stackMap = calculateStackMapFrames methodDesc code
        -- liftIO $ putStrLn $ showPretty stackMap
        -- stackMap `shouldNotBe` []
        hedgehog $ do
            (insts, cpState) <- runConv code
            insts /== []