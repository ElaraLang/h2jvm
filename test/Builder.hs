{-# LANGUAGE OverloadedLists #-}

module Builder where

import Control.Monad.IO.Class
import Data.Binary.Put
import Data.Binary.Write
import Data.ByteString qualified as BS
import Data.List (scanl')
import Hedgehog
import JVM.Data.Abstract.Builder (addMethod, runClassBuilder)
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type as JVM
import JVM.Data.Analyse.Instruction
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
    -- complex1
    simple1

-- complex2

simple1 :: Spec
simple1 = describe "Should handle another more simple example correctly" $ do
    let ((label1, absInsts), attrs, code) = runCodeBuilder' $ do
            label1 <- newLabel
            let code =
                    [ ALoad 0
                    , Goto label1
                    , Label label1
                    , AReturn
                    ]

            emit' code
            pure (label1, code)

    it "Converts correctly" $ hedgehog $ do
        (insts, _) <- runConv code
        insts
            === [ Raw.ALoad0
                , Raw.Goto 3
                , Raw.AReturn
                ]

    it "Calculates the stack map frames correctly" $ hedgehog $ do
        let desc = MethodDescriptor [(ObjectFieldType "java.lang.Integer")] (TypeReturn (ObjectFieldType "java.lang.Integer"))
            (_, _, diffs) = analyseStackMapTable desc code
            frames = calculateStackMapFrames desc code

        frames
            === [ SameLocals1StackItemFrame IntegerVariableInfo label1
                ]
        diffs
            === [ Just (StackPush [ObjectFieldType "java.lang.Integer"], LocalsSame)
                , Just (StackSame, LocalsSame)
                , Nothing
                , Just (StackPop 1, LocalsSame)
                ]

        applyMany (fmap fst <$> diffs) [] === []
        applyMany (fmap snd <$> diffs) (methodParams desc)
            === [(ObjectFieldType "java.lang.Integer")]

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
                    [ ALoad 0 -- stack = [java.lang.Integer]
                    , LDC (LDCInt 1) -- stack = [java.lang.Integer, 1]
                    , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer"))) -- stack = [java.lang.Integer, java.lang.Integer]
                    , InvokeStatic (ClassInfoType "Prelude") "eq" (MethodDescriptor [ObjectFieldType "java.lang.Object", ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Boolean"))) -- stack = [java.lang.Boolean]
                    , InvokeVirtual (ClassInfoType "java.lang.Boolean") "booleanValue" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Boolean))) -- stack = [java.lang.Boolean]
                    , IfEq label1 -- stack = []
                    , LDC (LDCInt 1) -- stack = [1]
                    , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer"))) -- stack = [java.lang.Integer]
                    , Goto label2 -- stack = [java.lang.Integer]
                    , Label label1 -- stack = [java.lang.Integer]
                    , ALoad 0 -- stack = [java.lang.Integer, java.lang.Integer]
                    , ALoad 0 -- stack = [java.lang.Integer, java.lang.Integer, java.lang.Integer]
                    , LDC (LDCInt 1) -- stack = [java.lang.Integer, java.lang.Integer, java.lang.Integer, 1]
                    , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer"))) -- stack = [java.lang.Integer, java.lang.Integer, java.lang.Integer, java.lang.Integer]
                    , InvokeStatic (ClassInfoType "Prelude") "minus" (MethodDescriptor [ObjectFieldType "java.lang.Integer", ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer"))) -- stack = [java.lang.Integer, java.lang.Integer, java.lang.Integer]
                    , InvokeStatic (ClassInfoType "fact") "fact" (MethodDescriptor [ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer"))) -- stack = [java.lang.Integer, java.lang.Integer]
                    , InvokeStatic (ClassInfoType "Prelude") "times" (MethodDescriptor [ObjectFieldType "java.lang.Integer", ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer"))) -- stack = [java.lang.Integer]
                    , Label label2 -- stack = [java.lang.Integer]
                    , AReturn -- stack = []
                    ]
            emit' code
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

    it "Calculates the stack map frames correctly" $ hedgehog $ do
        let desc = MethodDescriptor [ObjectFieldType "java.lang.Integer"] (TypeReturn (ObjectFieldType "java.lang.Integer"))
            (_, _, diffs) = analyseStackMapTable desc code
            frames = calculateStackMapFrames desc code

        diffs
            === ( Just
                    <$> [ (StackPush [ObjectFieldType "java.lang.Integer"], LocalsSame) -- stack = [java.lang.Integer]
                        , (StackPush [PrimitiveFieldType Int], LocalsSame) -- stack = [int, java.lang.Integer]
                        , (stackPopAndPush 1 [ObjectFieldType "java.lang.Integer"], LocalsSame) -- stack = [java.lang.Integer, java.lang.Integer]
                        , (stackPopAndPush 2 [ObjectFieldType "java.lang.Boolean"], LocalsSame) -- stack = [java.lang.Boolean]
                        , (stackPopAndPush 1 [PrimitiveFieldType Boolean], LocalsSame) -- stack = [boolean]
                        , (stackPop 1, LocalsSame) -- stack = []
                        , (StackPush [PrimitiveFieldType Int], LocalsSame) -- stack = [int]
                        , (stackPopAndPush 1 [ObjectFieldType "java.lang.Integer"], LocalsSame) -- stack = [java.lang.Integer]
                        , (StackSame, LocalsSame) -- stack = [java.lang.Integer]
                        ]
                )
            ++ [Nothing] -- stack = [java.lang.Integer]
            ++ ( Just
                    <$> [ (StackPush [ObjectFieldType "java.lang.Integer"], LocalsSame) -- stack = [java.lang.Integer, java.lang.Integer]
                        , (StackPush [ObjectFieldType "java.lang.Integer"], LocalsSame) -- stack = [java.lang.Integer, java.lang.Integer, java.lang.Integer]
                        , (StackPush [PrimitiveFieldType Int], LocalsSame) -- stack = [java.lang.Integer, java.lang.Integer, java.lang.Integer, int]
                        , (stackPopAndPush 1 [ObjectFieldType "java.lang.Integer"], LocalsSame) -- stack = [java.lang.Integer, java.lang.Integer, java.lang.Integer, java.lang.Integer]
                        , (stackPopAndPush 2 [ObjectFieldType "java.lang.Integer"], LocalsSame) -- stack = [java.lang.Integer, java.lang.Integer, java.lang.Integer]
                        , (stackPopAndPush 1 [ObjectFieldType "java.lang.Integer"], LocalsSame) -- stack = [java.lang.Integer, java.lang.Integer]
                        , (stackPopAndPush 2 [ObjectFieldType "java.lang.Integer"], LocalsSame) -- stack = [java.lang.Integer]
                        ]
               )
            ++ [ Nothing -- stack = [java.lang.Integer]
               , Just (stackPop 1, LocalsSame) -- stack = []
               ]

        frames
            === [ SameFrame label1
                , SameLocals1StackItemFrame (ObjectVariableInfo (ClassInfoType "java.lang.Integer")) label2
                ]

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
        (insts, _) <- runConv code
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

    it "Calculates the stack map frames correctly" $ hedgehog $ do
        let desc = MethodDescriptor [ObjectFieldType "elara.EList", ObjectFieldType "elara.EList"] (TypeReturn (ObjectFieldType "elara.EList"))
            (_, _, diffs) = analyseStackMapTable desc code
            frames = calculateStackMapFrames desc code

        let elist = ObjectVariableInfo (ClassInfoType "elara.EList")
        frames
            === [ FullFrame [elist] [elist, elist, elist, ObjectVariableInfo (ClassInfoType "java.lang.Object"), elist] label1
                , SameLocals1StackItemFrame elist label2
                ]
        diffs
            === ( Just
                    <$> [ (StackPush [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [elara.EList]
                        , (StackSame, LocalsSame) -- stack = [elara.EList]
                        , (StackPush [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [elara.EList, elara.EList]
                        , (stackPop 1, LocalsPush [ObjectFieldType "elara.EList"]) -- stack = [elara.EList]
                        , (stackPopAndPush 1 [PrimitiveFieldType Boolean], LocalsSame) -- stack = [Boolean]
                        , (stackPop 1, LocalsSame) -- stack = []
                        , (StackPush [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [elara.EList]
                        , (stackPopAndPush 1 [ObjectFieldType "java.lang.Object"], LocalsSame) -- stack = [java.lang.Object]
                        , (stackPop 1, LocalsPush [ObjectFieldType "java.lang.Object"]) -- stack = []
                        , (StackPush [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [elara.EList]
                        , (stackPopAndPush 1 [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [elara.EList]
                        , (stackPop 1, LocalsPush [ObjectFieldType "elara.EList"]) -- stack = []
                        , (StackPush [ObjectFieldType "java.lang.Object"], LocalsSame) -- stack = [java.lang.Object]
                        , (StackPush [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [java.lang.Object, elara.EList]
                        , (StackPush [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [java.lang.Object, elara.EList, elara.EList]
                        , (stackPopAndPush 2 [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [java.lang.Object, elara.EList]
                        , (stackPopAndPush 2 [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [elara.EList]
                        , (StackSame, LocalsSame) -- stack = [elara.EList]
                        ]
                )
            ++ [ Nothing -- stack = [elara.EList]
               , Just (StackPush [ObjectFieldType "elara.EList"], LocalsSame) -- stack = [elara.EList, elara.EList]
               , Nothing -- stack = [elara.EList]
               , Just (stackPop 1, LocalsSame) -- stack = [elara.EList]
               ]

        applyMany (fmap fst <$> diffs) [] === [ObjectFieldType "elara.EList"]
        applyMany (fmap snd <$> diffs) (methodParams desc)
            === replicate 3 (ObjectFieldType "elara.EList")
            ++ [ObjectFieldType "java.lang.Object"]
            ++ [ObjectFieldType "elara.EList"]

        let diffsUntilLabel1 = take 18 diffs
        applyMany (fmap fst <$> diffsUntilLabel1) [] === [ObjectFieldType "elara.EList"]
        applyMany (fmap snd <$> diffsUntilLabel1) (methodParams desc)
            === replicate 3 (ObjectFieldType "elara.EList")
            ++ [ObjectFieldType "java.lang.Object"]
            ++ [ObjectFieldType "elara.EList"]

complex3 :: Spec
complex3 = describe "Should handle another more simple example correctly" $ do
    {-
    public static void main(java.lang.String[]);
         0: ldc           #7                  // int 400043
         2: istore_1
         3: iconst_1
         4: istore_2
         5: iconst_1
         8: ifeq          22
        11: sipush        4922
        14: istore_3
        15: getstatic     #14                 // Field java/lang/System.out:Ljava/io/PrintStream;
        18: iload_3
        19: invokevirtual #20                 // Method java/io/PrintStream.println:(I)V
        22: ldc           #26                 // int 40303
        24: istore_3
        25: return
            -}
    let ((label1, absInsts), attrs, code) = runCodeBuilder' $ do
            label1 <- newLabel
            let code =
                    [ LDC (LDCInt 400043)
                    , IStore 1
                    , LDC (LDCInt 1)
                    , IStore 2
                    , LDC (LDCInt 1)
                    , IfEq label1
                    , LDC (LDCInt 4922)
                    , IStore 3
                    , GetStatic (ClassInfoType "java.lang.System") "out" (ObjectFieldType "java.io.PrintStream")
                    , ILoad 3
                    , InvokeVirtual (ClassInfoType "java.io.PrintStream") "println" (MethodDescriptor [PrimitiveFieldType JVM.Int] VoidReturn)
                    , Label label1
                    , LDC (LDCInt 40303)
                    , IStore 3
                    , Return
                    ]

            emit' code
            pure (label1, code)

    it "Converts correctly" $ hedgehog $ do
        (insts, _) <- runConv code
        insts
            === [ Raw.LDC 1 -- #0
                , Raw.IStore1 -- #2
                , Raw.LDC 2 -- #3
                , Raw.IStore2 -- #4
                , Raw.LDC 2 -- #5
                , Raw.IfEq 13 -- #8
                , Raw.LDC 3 -- #11
                , Raw.IStore3 -- #14
                , Raw.GetStatic 9 -- #15
                , Raw.ILoad3 -- #18
                , Raw.InvokeVirtual 15 -- #19
                , Raw.LDC 16 -- #22
                , Raw.IStore3 -- #24
                , Raw.Return -- #25
                ]

    it "Calculates the stack map frames correctly" $ hedgehog $ do
        let desc = MethodDescriptor [ArrayFieldType (ObjectFieldType "java.lang.String")] VoidReturn
            (_, _, diffs) = analyseStackMapTable desc code
            frames = calculateStackMapFrames desc code

        frames
            === [ AppendFrame [IntegerVariableInfo, IntegerVariableInfo] label1
                ]
        diffs
            === ( Just
                    <$> [ (StackPush [PrimitiveFieldType Int], LocalsSame) -- ldc 1 -> stack = [int] locals = [java.lang.String[]]
                        , (StackPop 1, LocalsPush [PrimitiveFieldType Int]) -- istore1 -> stack = [] locals = [java.lang.String[], int]
                        , (StackPush [PrimitiveFieldType Int], LocalsSame) -- ldc 2 -> stack = [int] locals = [java.lang.String[], int]
                        , (StackPop 1, LocalsPush [PrimitiveFieldType Int]) -- istore2 -> stack = [] locals = [java.lang.String[], int, int]
                        , (StackPush [PrimitiveFieldType Int], LocalsSame) -- ldc 2 -> stack = [int] locals = [java.lang.String[], int, int]
                        , (StackPop 1, LocalsSame) -- ifeq 13 -> stack = []    locals = [java.lang.String[], int, int]
                        , (StackPush [PrimitiveFieldType Int], LocalsSame) -- ldc 3 -> stack = [int] locals = [java.lang.String[], int, int]
                        , (StackPop 1, LocalsPush [PrimitiveFieldType Int]) -- istore3 -> stack = [] locals = [java.lang.String[], int, int, int]
                        , (StackPush [ObjectFieldType "java.io.PrintStream"], LocalsSame) -- getstatic -> stack = [java.io.PrintStream] locals = [java.lang.String[], int, int, int]
                        , (StackPush [PrimitiveFieldType Int], LocalsSame) -- iload3 -> stack = [int, java.io.PrintStream] locals = [java.lang.String[], int, int, int]
                        , (StackPop 2, LocalsSame) -- invokevirtual -> stack = []  locals = [java.lang.String[], int, int, int]
                        ]
                )
            ++ [ Nothing -- stack = [] locals = [java.lang.String[], int, int, int]
               , Just (StackPush [PrimitiveFieldType Int], LocalsSame) -- ldc 16 -> stack = [int] locals = [java.lang.String[], int, int, int]
               , Just (StackPop 1, LocalsSame) -- istore3 -> stack = [] locals = [java.lang.String[], int, int, int]
               , Just (StackSame, LocalsSame) -- stack = [] locals = [java.lang.String[], int, int, int]
               ]

        applyMany (fmap fst <$> diffs) [] === []
        applyMany (fmap snd <$> diffs) (methodParams desc)
            === [ArrayFieldType (ObjectFieldType "java.lang.String"), PrimitiveFieldType Int, PrimitiveFieldType Int, PrimitiveFieldType Int]
