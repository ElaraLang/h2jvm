module Builder where

import Data.Binary.Put
import Data.Binary.Write
import Data.ByteString qualified as BS
import Hedgehog
import JVM.Data.Abstract.Builder (addMethod, runClassBuilder)
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type as JVM
import JVM.Data.Analyse.Instruction (calculateStackMapFrames)
import JVM.Data.Convert
import JVM.Data.JVMVersion (java17)
import JVM.Data.Raw.Instruction qualified as Raw
import Test.Hspec
import Test.Hspec.Hedgehog
import Util (runConv, shouldBeRight)

spec :: Spec
spec = describe "test code building" $ do
    it "Should handle simple labels correctly" $ do
        {-
        0: aload_0
        1: ifeq          4
        Label #1
        4: return
        -}
        let (_, code) = runCodeBuilder $ do
                label <- newLabel
                emit $ ALoad 0
                emit (IfEq label)
                emit (Label label)
                emit Return
        (insts, _) <- runConv code
        insts
            `shouldBe` [ Raw.ALoad0
                       , Raw.IfEq 3
                       , Raw.Return
                       ]

    describe "Should handle more complex labels correctly" $ do
        {-
         0: aload_0
         1: ldc           #39                 // int 0
         3: invokestatic  #15                 // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
         6: invokestatic  #43                 // Method Prelude.eq:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Boolean;
         9: invokevirtual #49                 // Method java/lang/Boolean.booleanValue:()Z
        12: ifeq          23
        15: ldc           #50                 // int 1
        17: invokestatic  #15                 // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
        20: goto          39
        Label #1
        23: aload_0
        24: aload_0
        25: ldc           #50                 // int 1
        27: invokestatic  #15                 // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
        30: invokestatic  #54                 // Method Prelude.minus:(Ljava/lang/Integer;Ljava/lang/Integer;)Ljava/lang/Integer;
        33: invokestatic  #19                 // Method fact:(Ljava/lang/Integer;)Ljava/lang/Integer;
        36: invokestatic  #57                 // Method Prelude.times:(Ljava/lang/Integer;Ljava/lang/Integer;)Ljava/lang/Integer;
        Label #2
        39: areturn
        -}
        let (((label1, label2), absInsts), attrs, code) = runCodeBuilder' $ do
                label1 <- newLabel
                label2 <- newLabel
                let code =
                        [ ALoad 0
                        , LDC (LDCInt 1)
                        , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer")))
                        , InvokeStatic (ClassInfoType "Prelude") "eq" (MethodDescriptor [ObjectFieldType "java.lang.Object", ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Boolean")))
                        , InvokeVirtual (ClassInfoType "java.lang.Boolean") "booleanValue" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Boolean)))
                        , IfEq label1
                        , LDC (LDCInt 1)
                        , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer")))
                        , Goto label2
                        , Label label1
                        , ALoad 0
                        , ALoad 0
                        , LDC (LDCInt 1)
                        , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer")))
                        , InvokeStatic (ClassInfoType "Prelude") "minus" (MethodDescriptor [ObjectFieldType "java.lang.Integer", ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer")))
                        , InvokeStatic (ClassInfoType "fact") "fact" (MethodDescriptor [ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer")))
                        , InvokeStatic (ClassInfoType "Prelude") "times" (MethodDescriptor [ObjectFieldType "java.lang.Integer", ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer")))
                        , Label label2
                        , AReturn
                        ]
                emit' code
                appendStackMapFrame (SameFrame label1)
                appendStackMapFrame (SameFrame label2)
                pure ((label1, label2), code)
        it "Converts correctly" $ do
            (insts, _) <- runConv code
            insts
                `shouldBe` [ Raw.ALoad0 -- #0
                           , Raw.LDC 1 -- #1
                           , Raw.InvokeStatic 7 -- #3
                           , Raw.InvokeStatic 13 -- #6
                           , Raw.InvokeVirtual 19 -- #9
                           , Raw.IfEq 11 -- #12
                           , Raw.LDC 1 -- #15
                           , Raw.InvokeStatic 7 -- #17
                           , Raw.Goto 19 -- #20
                           , Raw.ALoad0 -- #23
                           , Raw.ALoad0 -- #24
                           , Raw.LDC 1 -- #25
                           , Raw.InvokeStatic 7 -- #27
                           , Raw.InvokeStatic 23 -- #30
                           , Raw.InvokeStatic 28 -- #33
                           , Raw.InvokeStatic 31 -- #36
                           , Raw.AReturn -- #39
                           ]

        it "Calculates the stack map frames correctly" $ do
            hedgehog $ do
                let desc = MethodDescriptor [ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer"))
                    frames = calculateStackMapFrames desc code

                frames
                    === [ SameFrame label1
                        , SameLocals1StackItemFrame (ObjectVariableInfo (ClassInfoType "java.lang.Integer")) label2
                        ]

        it "Writes to a file without issue" $ do
            let (_, clazz) =
                    runClassBuilder "BuilderTest" java17 $
                        addMethod $
                            ClassFileMethod
                                [MPublic, MStatic]
                                "main"
                                (MethodDescriptor [ObjectFieldType "java.lang.String"] VoidReturn)
                                [ Code $
                                    CodeAttributeData
                                        5
                                        2
                                        absInsts
                                        []
                                        attrs
                                ]

            let classFile' = convert clazz

            classContents <- shouldBeRight classFile'
            let bs = runPut (writeBinary classContents)

            BS.writeFile "BuilderTest.class" (BS.toStrict bs)
