{-# LANGUAGE LambdaCase #-}

module Convert where

import Data.Vector
import JVM.Data.Abstract.ConstantPool (runConstantPoolM)
import JVM.Data.Abstract.Descriptor (MethodDescriptor (MethodDescriptor), ReturnDescriptor (Return))
import JVM.Data.Abstract.Instruction (Instruction (InvokeStatic))
import JVM.Data.Abstract.Type (ClassInfoType (ClassInfoType), FieldType (PrimitiveFieldType), PrimitiveType (Char, Int))
import JVM.Data.Convert.Instruction (convertInstruction)
import JVM.Data.Raw.ConstantPool (ConstantPoolInfo (..))
import JVM.Data.Raw.ConstantPool qualified as Raw
import JVM.Data.Raw.Instruction qualified as Raw
import Test.Hspec hiding (shouldContain)
import Util (shouldBeJust, shouldContain)

spec :: Spec
spec = describe "test conversions" $ do
    it "Converts a simple invokestatic instruction properly" $ do
        let (inst, constants) =
                runConstantPoolM $
                    convertInstruction
                        ( InvokeStatic
                            (ClassInfoType "java.lang.String")
                            "charAt"
                            (MethodDescriptor [PrimitiveFieldType Int] (Return (PrimitiveFieldType Char)))
                        )
        constants `shouldContain` Raw.UTF8Info "java/lang/String"
        constants `shouldContain` Raw.UTF8Info "charAt"
        constants `shouldContain` Raw.UTF8Info "(I)C"

        indexOfMethodRef <- shouldBeJust $ findIndex (\case MethodRefInfo _ _ -> True; _ -> False) constants
        inst `shouldBe` Raw.InvokeStatic (fromIntegral indexOfMethodRef + 1)
