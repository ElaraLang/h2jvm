module Builder where

import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.Instruction
import JVM.Data.Raw.Instruction qualified as Raw
import Test.Hspec
import Util (runConv)

spec :: Spec
spec = describe "test code building" $ do
    it "Should handle labels correctly" $ do
        let code = runCodeBuilder $ do
                label <- newLabel
                emit ALoad0
                emit (IfEq label)
                emit (Label label)
                emit Return
        (insts, cp) <- runConv code
        insts
            `shouldBe` [ Raw.ALoad0
                       , Raw.IfEq 4
                       , Raw.Return
                       ]
