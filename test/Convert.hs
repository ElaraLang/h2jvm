{-# LANGUAGE LambdaCase #-}

module Convert where

import Data.Vector
import JVM.Data.Abstract.ConstantPool (runConstantPoolM)
import JVM.Data.Abstract.Descriptor (MethodDescriptor (MethodDescriptor), ReturnDescriptor (TypeReturn))
import JVM.Data.Abstract.Instruction (Instruction (..), LDCEntry (..))
import JVM.Data.Abstract.Type (ClassInfoType (ClassInfoType), FieldType (..), PrimitiveType (Char, Int))
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
                  (MethodDescriptor [PrimitiveFieldType Int] (TypeReturn (PrimitiveFieldType Char)))
              )
    constants `shouldContain` Raw.UTF8Info "java/lang/String"
    constants `shouldContain` Raw.UTF8Info "charAt"
    constants `shouldContain` Raw.UTF8Info "(I)C"

    indexOfMethodRef <- shouldBeJust $ findIndex (\case MethodRefInfo _ _ -> True; _ -> False) constants
    inst `shouldBe` Raw.InvokeStatic (fromIntegral indexOfMethodRef + 1)

  it "Converts a simple ldc instruction properly" $ do
    let (inst, constants) = runConstantPoolM $ convertInstruction (LDC $ LDCString "hello")
    constants `shouldContain` Raw.UTF8Info "hello"
    indexOfInteger <- shouldBeJust $ findIndex (\case StringInfo _ -> True; _ -> False) constants
    inst `shouldBe` Raw.LDC (fromIntegral indexOfInteger + 1)
