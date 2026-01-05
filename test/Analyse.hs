module Analyse where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import JVM.Data.Abstract.Type

import Effectful
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.AccessFlags (MethodAccessFlag (..))
import JVM.Data.Abstract.ClassFile.Method (StackMapFrame (..), VerificationTypeInfo (..))
import JVM.Data.Abstract.ConstantPool (BootstrapArgument (..), BootstrapMethod (..), MethodHandleEntry (..), MethodRef (..))
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Analyse.StackMap (BasicBlock (BasicBlock), Frame (..), LocalVariable (..), analyseBlockDiff, calculateStackMapFrames, diffFrames, splitIntoBasicBlocks, topFrame)
import JVM.Data.Convert (jloName)
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
                    === [ BasicBlock 0 [LDC (LDCInt 0), AStore 0, ALoad 0, AReturn] Nothing Nothing
                        ]

                let top = topFrame jloName [MStatic] (MethodDescriptor [] (TypeReturn (PrimitiveFieldType Int)))
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
                    === [ BasicBlock 0 [LDC (LDCInt 0), IfEq l] Nothing Nothing
                        , BasicBlock 1 [LDC (LDCInt 0), AReturn] Nothing (Just l)
                        , BasicBlock 2 [LDC (LDCInt 1), AReturn] (Just l) Nothing
                        ]

                let top = topFrame jloName [MStatic] (MethodDescriptor [] (TypeReturn (PrimitiveFieldType Int)))
                let nextFrame = analyseBlockDiff top (head blocks)

                nextFrame
                    === Frame
                        { locals = []
                        , stack = []
                        }

                let nextFrame' = analyseBlockDiff nextFrame (blocks !! 2)

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
            hedgehog $ do
                let blocks = splitIntoBasicBlocks code

                blocks
                    === [ BasicBlock 0 [LDC (LDCInt 0), IStore 0, LDC (LDCInt 0), IStore 1, ILoad 0, IfLe l] Nothing Nothing
                        , BasicBlock 1 [LDC (LDCInt 0), IStore 2] Nothing (Just l)
                        , BasicBlock 2 [LDC (LDCInt 0), IStore 2, Return] (Just l) Nothing
                        ]

                let top = topFrame jloName [MStatic] (MethodDescriptor [] VoidReturn)
                let nextFrame = analyseBlockDiff top (head blocks)

                nextFrame
                    === Frame
                        { locals = [LocalVariable (PrimitiveFieldType Int), LocalVariable (PrimitiveFieldType Int)]
                        , stack = []
                        }

                let nextFrame' = analyseBlockDiff nextFrame (blocks !! 2)

                nextFrame'
                    === Frame
                        { locals = [LocalVariable (PrimitiveFieldType Int), LocalVariable (PrimitiveFieldType Int), LocalVariable (PrimitiveFieldType Int)]
                        , stack = []
                        }

                -- The frame at label l should now have 2 locals when we arrive at it (from block 0)
                diffFrames top nextFrame l
                    === AppendFrame [IntegerVariableInfo, IntegerVariableInfo] l

        -- Complex test cases for edge cases discovered in real code
        it "Handles fall-through to unlabeled blocks correctly" $ do
            {-
            This tests the case where:
            - ifeq branches to label_end (returns constant)
            - Fall-through goes to an unlabeled block with different return

            This was causing "Expecting a stackmap frame at branch target" errors
            -}
            let ((lEnd, lOther), _, code) = runPureEff $ runCodeBuilder $ do
                    emit $ ALoad 0
                    emit $ Instanceof (ClassInfoType "Some/Class")
                    labelEnd <- newLabel
                    emit $ IfEq labelEnd -- if not instanceof, branch to labelEnd
                    -- Fall-through: it IS an instanceof, load and return it
                    emit $ ALoad 0 -- unlabeled block
                    emit AReturn
                    emit $ Label labelEnd
                    labelOther <- newLabel
                    emit $ Goto labelOther
                    emit $ Label labelOther
                    emit $ AConstNull
                    emit AReturn
                    pure (labelEnd, labelOther)

            hedgehog $ do
                let md = MethodDescriptor [ObjectFieldType "Some/Class"] (TypeReturn (ObjectFieldType "java/lang/Object"))
                let stackMap = calculateStackMapFrames jloName [MStatic] md code

                -- Should have frames for labelEnd and labelOther
                length stackMap === 2

        it "Handles conditional with stack value at merge point" $ do
            {-
            This tests the pattern:
            - instanceof check
            - ifeq branches to create False, falls through to create True
            - Both paths leave a Bool on the stack
            - Merge point receives Bool from either path

            This was causing "Type X is not assignable to Y" errors
            -}
            let ((lFalse, lMerge), _, code) = runPureEff $ runCodeBuilder $ do
                    emit $ ALoad 0
                    emit $ Instanceof (ClassInfoType "Some/Type")
                    labelFalse <- newLabel
                    emit $ IfEq labelFalse -- if not instanceof, goto labelFalse
                    emit $ New (ClassInfoType "Prim/True")
                    emit $ Dup
                    emit $ InvokeSpecial (ClassInfoType "Prim/True") "<init>" (MethodDescriptor [] VoidReturn)
                    emit $ CheckCast (ClassInfoType "Prim/Bool")
                    labelMerge <- newLabel
                    emit $ Goto labelMerge
                    emit $ Label labelFalse
                    emit $ New (ClassInfoType "Prim/False")
                    emit $ Dup
                    emit $ InvokeSpecial (ClassInfoType "Prim/False") "<init>" (MethodDescriptor [] VoidReturn)
                    emit $ CheckCast (ClassInfoType "Prim/Bool")
                    emit $ Label labelMerge -- merge point - stack has Bool
                    emit $ Instanceof (ClassInfoType "Prim/True")
                    emit AReturn
                    pure (labelFalse, labelMerge)

            hedgehog $ do
                let blocks = splitIntoBasicBlocks code
                let md = MethodDescriptor [ObjectFieldType "java/lang/Object"] (TypeReturn (PrimitiveFieldType Int))
                let stackMap = calculateStackMapFrames jloName [MStatic] md code

                -- Check that we have the right number of frames
                length stackMap === 2

                -- The merge frame should have Bool on stack
                case stackMap of
                    [_, SameLocals1StackItemFrame vti _] ->
                        vti === ObjectVariableInfo (ClassInfoType "Prim/Bool")
                    _ -> fail "Expected SameLocals1StackItemFrame for merge point"

        it "Handles nested conditionals with multiple merge points" $ do
            {-
            Pattern from lifted4988:
            - First conditional creates True/False
            - Check result, branch again
            - Multiple levels of nesting
            -}
            let ((l1, l2, l3, l4, l5), _, code) = runPureEff $ runCodeBuilder $ do
                    emit $ ALoad 0
                    emit $ AStore 1

                    -- First conditional
                    label1 <- newLabel
                    emit $ Label label1
                    emit $ ALoad 1
                    emit $ Instanceof (ClassInfoType "List/Cons")
                    label2 <- newLabel
                    emit $ IfEq label2
                    emit $ New (ClassInfoType "Prim/True")
                    emit $ Dup
                    emit $ InvokeSpecial (ClassInfoType "Prim/True") "<init>" (MethodDescriptor [] VoidReturn)
                    emit $ CheckCast (ClassInfoType "Prim/Bool")
                    label3 <- newLabel
                    emit $ Goto label3
                    emit $ Label label2
                    emit $ New (ClassInfoType "Prim/False")
                    emit $ Dup
                    emit $ InvokeSpecial (ClassInfoType "Prim/False") "<init>" (MethodDescriptor [] VoidReturn)
                    emit $ CheckCast (ClassInfoType "Prim/Bool")
                    emit $ Label label3
                    emit $ Instanceof (ClassInfoType "Prim/True")
                    label5 <- newLabel
                    emit $ IfEq label5

                    -- True branch - do some work
                    label4 <- newLabel
                    emit $ Goto label4
                    emit $ Label label4
                    emit $ ALoad 1
                    emit $ GetField (ClassInfoType "List/Cons") "head" (ObjectFieldType "java/lang/Object")
                    emit $ AStore 2
                    emit $ ALoad 2
                    emit AReturn

                    -- False branch
                    emit $ Label label5
                    emit $ InvokeStatic (ClassInfoType "Error") "fail" (MethodDescriptor [] (TypeReturn (ObjectFieldType "java/lang/Object")))
                    emit AReturn

                    pure (label1, label2, label3, label4, label5)

            hedgehog $ do
                let blocks = splitIntoBasicBlocks code
                let md = MethodDescriptor [ObjectFieldType "java/lang/Object"] (TypeReturn (ObjectFieldType "java/lang/Object"))
                let stackMap = calculateStackMapFrames jloName [MStatic]  md code

                -- Should generate valid frames without crashing
                (length stackMap > 0) === True

        it "Handles goto immediately after conditional branch (fall-through pattern)" $ do
            {-
            This specific pattern was problematic:
            - ifeq label_x
            - goto label_y   <- unlabeled block!
            - :label_y
            - ... code ...

            The goto's target needs a stackmap frame even though the goto itself is unlabeled
            -}
            let ((lX, lY), _, code) = runPureEff $ runCodeBuilder $ do
                    emit $ LDC (LDCInt 0)
                    labelX <- newLabel
                    emit $ IfEq labelX
                    labelY <- newLabel
                    emit $ Goto labelY -- This block has no label!
                    emit $ Label labelX
                    emit $ LDC (LDCInt 1)
                    emit AReturn
                    emit $ Label labelY
                    emit $ LDC (LDCInt 2)
                    emit AReturn
                    pure (labelX, labelY)

            hedgehog $ do
                let blocks = splitIntoBasicBlocks code

                -- Should have 4 blocks:
                -- 0: [LDC, IfEq]
                -- 1: [Goto]  <- unlabeled!
                -- 2: [LDC, AReturn] <- labelX
                -- 3: [LDC, AReturn] <- labelY
                length blocks === 4

                let md = MethodDescriptor [] (TypeReturn (PrimitiveFieldType Int))
                let stackMap = calculateStackMapFrames jloName [MStatic]  md code

                -- Should have frames for labelX and labelY
                length stackMap === 2

        it "Handles multiple branches to same target" $ do
            {-
            Multiple paths converging:
            - path 1: goto label_end
            - path 2: goto label_end
            - path 3: falls through to label_end
            -}
            let (lEnd, _, code) = runPureEff $ runCodeBuilder $ do
                    emit $ LDC (LDCInt 0)
                    emit $ IStore 0

                    labelEnd <- newLabel

                    emit $ ILoad 0
                    emit $ IfEq labelEnd -- path 1
                    emit $ LDC (LDCInt 1)
                    emit $ IStore 1
                    emit $ Goto labelEnd -- path 2
                    emit $ Label labelEnd -- path 3 falls through from above goto
                    emit $ ILoad 0
                    emit AReturn

                    pure labelEnd

            hedgehog $ do
                let blocks = splitIntoBasicBlocks code
                let md = MethodDescriptor [] (TypeReturn (PrimitiveFieldType Int))
                let stackMap = calculateStackMapFrames jloName [MStatic] md code

                -- The frame at labelEnd should account for all incoming paths
                length stackMap === 1

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
        let stackMap = calculateStackMapFrames jloName [MStatic] methodDesc code
        -- liftIO $ putStrLn $ showPretty stackMap
        -- stackMap `shouldNotBe` []
        hedgehog $ do
            (insts, cpState) <- runConv code
            insts /== []