{-# LANGUAGE OverloadedLists #-}

module Builder where

import Hedgehog
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.Instruction
import JVM.Data.Raw.Instruction qualified as Raw
import Polysemy
import Test.Hspec
import Test.Hspec.Hedgehog
import Util (runConv)

spec :: Spec
spec = describe "test code building" $ do
    it "Should handle simple labels correctly" $ do
        {-
        0: aload_0
        1: ifeq          4
        Label #1
        4: return
        -}
        let (_, _, code) = run $ runCodeBuilder $ do
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
