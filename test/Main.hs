{-# LANGUAGE OverloadedLists #-}

module Main where

import Analyse qualified (spec)
import Builder qualified (spec)
import Convert qualified (spec)
import Test.Hspec (Spec, hspec)

spec :: Spec
spec = do
    Convert.spec
    Builder.spec
    Analyse.spec

main :: IO ()
main = do
    hspec spec
