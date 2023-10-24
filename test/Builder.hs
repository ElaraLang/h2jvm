module Builder where

import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.Instruction
import Test.Hspec
import JVM.Data.Convert.Instruction (convertInstruction)
import JVM.Data.Convert.ConstantPool (runConstantPoolM)

spec :: Spec
spec = describe "test code building" $ do
    let code = runCodeBuilder $ do
            label <- newLabel
            emit ALoad0
            emit (IfEq label)
            emit (Label label)
            emit Return
    let x = runConstantPoolM $ traverse convertInstruction code
    runIO $ print x
