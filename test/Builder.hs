{-# LANGUAGE OverloadedLists #-}

module Builder where

import Effectful
import Hedgehog
import Test.Syd
import Test.Syd.Hedgehog ()

import H2JVM.Builder.Code
import H2JVM.Instruction
import Util (runConv)

import H2JVM.Internal.Raw.Instruction qualified as Raw

spec :: Spec
spec = describe "test code building" $ do
    it "Should handle simple labels correctly" $ do
        {-
        0: aload_0
        1: ifeq          4
        Label #1
        4: return
        -}
        let (_, _, code) = runPureEff $ runCodeBuilder $ do
                label <- newLabel
                emit $ ALoad 0
                emit (If $ IfEq label)
                emit (Label label)
                emit Return
        property $ do
            (insts, _) <- runConv code
            insts
                === [ Raw.ALoad0
                    , Raw.IfEq 3
                    , Raw.Return
                    ]
