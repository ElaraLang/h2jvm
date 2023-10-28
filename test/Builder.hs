module Builder where

import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type as JVM
import JVM.Data.Raw.Instruction qualified as Raw
import Test.Hspec
import Util (runConv)

spec :: Spec
spec = describe "test code building" $ do
    it "Should handle simple labels correctly" $ do
        let code = runCodeBuilder $ do
                label <- newLabel
                emit ALoad0
                emit (IfEq label)
                emit (Label label)
                emit Return
        (insts, _) <- runConv code
        insts
            `shouldBe` [ Raw.ALoad0
                       , Raw.IfEq 4
                       , Raw.Return
                       ]

    it "Should handle more complex labels correctly" $ do
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
        23: aload_0
        24: aload_0
        25: ldc           #50                 // int 1
        27: invokestatic  #15                 // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
        30: invokestatic  #54                 // Method Prelude.minus:(Ljava/lang/Integer;Ljava/lang/Integer;)Ljava/lang/Integer;
        33: invokestatic  #19                 // Method fact:(Ljava/lang/Integer;)Ljava/lang/Integer;
        36: invokestatic  #57                 // Method Prelude.times:(Ljava/lang/Integer;Ljava/lang/Integer;)Ljava/lang/Integer;
        39: areturn
        -}
        let code = runCodeBuilder $ do
                label1 <- newLabel
                label2 <- newLabel
                emit ALoad0
                emit (LDC (LDCInt 1))
                emit (InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer"))))
                emit (InvokeStatic (ClassInfoType "Prelude") "eq" (MethodDescriptor [ObjectFieldType "java.lang.Object", ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Boolean"))))
                emit (InvokeVirtual (ClassInfoType "java.lang.Boolean") "booleanValue" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Boolean))))
                emit (IfEq label1)
                emit (LDC (LDCInt 1))
                emit (InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer"))))
                emit (Goto label2)
                emit (Label label1)
                emit ALoad0
                emit ALoad0
                emit (LDC (LDCInt 1))
                emit (InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer"))))
                emit (InvokeStatic (ClassInfoType "Prelude") "minus" (MethodDescriptor [ObjectFieldType "java.lang.Integer", ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer"))))
                emit (InvokeStatic (ClassInfoType "fact") "fact" (MethodDescriptor [ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer"))))
                emit (InvokeStatic (ClassInfoType "Prelude") "times" (MethodDescriptor [ObjectFieldType "java.lang.Integer", ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer"))))
                emit (Label label2)
                emit AReturn
        (insts, _) <- runConv code
        insts
            `shouldBe` [ Raw.ALoad0 -- #0
                       , Raw.LDC 1 -- #1
                       , Raw.InvokeStatic 7 -- #3
                       , Raw.InvokeStatic 13 -- #6
                       , Raw.InvokeVirtual 19 -- #9
                       , Raw.IfEq 23 -- #12
                       , Raw.LDC 1 -- #15
                       , Raw.InvokeStatic 7 -- #17
                       , Raw.Goto 39 -- #20
                       , Raw.ALoad0 -- #23
                       , Raw.ALoad0 -- #24
                       , Raw.LDC 1 -- #25
                       , Raw.InvokeStatic 7 -- #27
                       , Raw.InvokeStatic 23 -- #30
                       , Raw.InvokeStatic 28 -- #33
                       , Raw.InvokeStatic 31 -- #36
                       , Raw.AReturn -- #39
                       ]
