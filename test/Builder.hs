module Builder where

import Control.Monad.IO.Class
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
        hedgehog $ do
            (insts, _) <- runConv code
            insts
                === [ Raw.ALoad0
                    , Raw.IfEq 3
                    , Raw.Return
                    ]
    complex1
    complex2

complex1 :: Spec
complex1 = describe "Should handle more complex labels correctly" $ do
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
    it "Converts correctly" $ hedgehog $ do
        (insts, _) <- runConv code
        insts
            === [ Raw.ALoad0 -- #0
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

    it "Writes to a file without issue" $ hedgehog $ do
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

        liftIO $ BS.writeFile "BuilderTest.class" (BS.toStrict bs)

complex2 :: Spec
complex2 = describe "Should handle another complex example correctly" $ do
    {-
    public static elara.EList plusplus(elara.EList, elara.EList);
      Code:
         0: aload_0
         1: checkcast     #12                 // class elara/EList
         4: dup
         5: astore_2
         6: invokevirtual #16                 // Method elara/EList.isEmpty:()Z
         9: ifne          36
        12: aload_2
        13: getfield      #20                 // Field elara/EList.head:Ljava/lang/Object;
        16: astore_3
        17: aload_2
        18: getfield      #24                 // Field elara/EList.tail:Lelara/EList;
        21: astore        4
        23: aload_3
        24: aload         4
        26: aload_1
        27: invokestatic  #26                 // Method plusplus:(Lelara/EList;Lelara/EList;)Lelara/EList;
        30: invokestatic  #32                 // Method Elara/Prim.cons:(Ljava/lang/Object;Lelara/EList;)Lelara/EList;
        33: goto          37
        36: aload_1
        37: areturn
          -}
    let (((label1, label2), absInsts), attrs, code) = runCodeBuilder' $ do
            label1 <- newLabel
            label2 <- newLabel
            let code =
                    [ ALoad 0
                    , CheckCast (ClassInfoType "elara.EList")
                    , Dup
                    , AStore 2
                    , InvokeVirtual (ClassInfoType "elara.EList") "isEmpty" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Boolean)))
                    , IfNe label1
                    , ALoad 2
                    , GetField (ClassInfoType "elara.EList") "head" (ObjectFieldType "java.lang.Object")
                    , AStore 3
                    , ALoad 2
                    , GetField (ClassInfoType "elara.EList") "tail" (ObjectFieldType "elara.EList")
                    , AStore 4
                    , ALoad 3
                    , ALoad 4
                    , ALoad 1
                    , InvokeStatic (ClassInfoType "BuilderTest2") "plusplus" (MethodDescriptor [ObjectFieldType "elara.EList", ObjectFieldType "elara.EList"] (TypeReturn (ObjectFieldType "elara.EList")))
                    , InvokeStatic (ClassInfoType "Elara.Prim") "cons" (MethodDescriptor [ObjectFieldType "java.lang.Object", ObjectFieldType "elara.EList"] (TypeReturn (ObjectFieldType "elara.EList")))
                    , Goto label2
                    , Label label1
                    , ALoad 1
                    , Label label2
                    , AReturn
                    ]

            emit' code
            pure ((label1, label2), code)

    
    it "Converts correctly" $ hedgehog $ do

        (insts, _) <-  runConv code
        insts
            === [ Raw.ALoad0 -- #0
                , Raw.CheckCast 2 -- #1
                , Raw.Dup -- #4
                , Raw.AStore2 -- #5
                , Raw.InvokeVirtual 6 -- #6
                , Raw.IfNe 27 -- #9
                , Raw.ALoad2 -- #12
                , Raw.GetField 10 -- #13
                , Raw.AStore3 -- #16
                , Raw.ALoad2 -- #17
                , Raw.GetField 14 -- #18
                , Raw.AStore 4 -- #21
                , Raw.ALoad3 -- #23
                , Raw.ALoad 4 -- #24
                , Raw.ALoad1 -- #25
                , Raw.InvokeStatic 20 -- #27
                , Raw.InvokeStatic 26 -- #30
                , Raw.Goto 4 -- #33
                , Raw.ALoad1 -- #36
                , Raw.AReturn -- #37
                ]

    it "Calculates the stack map frames correctly" $ do
        hedgehog $ do
            let desc = MethodDescriptor [ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer"))
                frames = calculateStackMapFrames desc code

            frames
                === [ SameFrame label1
                    , SameLocals1StackItemFrame (ObjectVariableInfo (ClassInfoType "java.lang.Integer")) label2
                    ]

    it "Writes to a file without issue" $ hedgehog $ do
        let (_, clazz) =
                runClassBuilder "BuilderTest2" java17 $
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

        liftIO $ BS.writeFile "BuilderTest2.class" (BS.toStrict bs)
