{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Convert where

import Data.IndexedMap qualified as IM
import JVM.Data.Abstract.ConstantPool
import JVM.Data.Abstract.Descriptor (MethodDescriptor (MethodDescriptor), ReturnDescriptor (TypeReturn))
import JVM.Data.Abstract.Instruction (Instruction (..), LDCEntry (..))
import JVM.Data.Abstract.Type (ClassInfoType (ClassInfoType), FieldType (..), PrimitiveType (Boolean, Char, Int))
import JVM.Data.Convert.ConstantPool (ConstantPoolState (ConstantPoolState), runConstantPoolM)
import JVM.Data.Convert.Instruction (convertInstruction)
import JVM.Data.Raw.ClassFile qualified as Raw
import JVM.Data.Raw.ConstantPool (ConstantPoolInfo (..))
import JVM.Data.Raw.ConstantPool qualified as Raw
import JVM.Data.Raw.Instruction qualified as Raw
import Test.Hspec hiding (shouldContain)
import Util (shouldBeJust, shouldContain)

spec :: Spec
spec = describe "test conversions" $ do
    it "Converts a simple invokestatic instruction properly" $ do
        let (inst, ConstantPoolState constants bms) =
                runConstantPoolM $
                    convertInstruction
                        ( InvokeStatic
                            (ClassInfoType "java.lang.String")
                            "charAt"
                            (MethodDescriptor [PrimitiveFieldType Int] (TypeReturn (PrimitiveFieldType Char)))
                        )
        constants `shouldContain` Raw.UTF8Info "java/lang/String"
        constants `shouldContain` Raw.UTF8Info "charAt"
        constants `shouldContain` Raw.UTF8Info "(I)C"

        bms `shouldBe` []

        indexOfMethodRef <- findCPIndex (\case MethodRefInfo _ _ -> True; _ -> False) constants
        inst `shouldBe` Raw.InvokeStatic (fromIntegral indexOfMethodRef )

    it "Converts a simple ldc instruction properly" $ do
        let (inst, ConstantPoolState constants bms) = runConstantPoolM $ convertInstruction (LDC $ LDCString "hello")
        constants `shouldContain` Raw.UTF8Info "hello"
        bms `shouldBe` []
        indexOfInteger <- findCPIndex (\case StringInfo _ -> True; _ -> False) constants
        inst `shouldBe` Raw.LDC (fromIntegral indexOfInteger)

    it "Converts a simple indy instruction properly" $ do
        let (inst, ConstantPoolState constants bms) =
                runConstantPoolM $
                    convertInstruction
                        ( InvokeDynamic
                            ( BootstrapMethod
                                ( MHInvokeStatic
                                    ( MethodRef
                                        (ClassInfoType "java/lang/Object")
                                        "equals"
                                        (MethodDescriptor [ObjectFieldType "java/lang/Object"] (TypeReturn (PrimitiveFieldType Boolean)))
                                    )
                                )
                                [BMStringArg "Hello"]
                            )
                            "foo"
                            (MethodDescriptor [PrimitiveFieldType Int] (TypeReturn (PrimitiveFieldType Char)))
                        )
        constants `shouldContain` Raw.UTF8Info "foo"
        constants `shouldContain` Raw.UTF8Info "(I)C"

        indexOfMethodHandle <- findCPIndex (\case MethodHandleInfo _ _ -> True; _ -> False) constants
        indexOfIndy <- findCPIndex (\case InvokeDynamicInfo _ _ -> True; _ -> False) constants
        strArgIndex <- findCPIndex (\case StringInfo _ -> True; _ -> False) constants


        bms `shouldBe` IM.singleton (Raw.BootstrapMethod (fromIntegral indexOfMethodHandle) [fromIntegral strArgIndex])
        inst `shouldBe` Raw.InvokeDynamic (fromIntegral indexOfIndy)

findCPIndex :: (a -> Bool) -> IM.IndexedMap a -> IO Int
findCPIndex pred cp = shouldBeJust $ IM.lookupIndexWhere pred cp