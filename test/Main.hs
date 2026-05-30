module Main where

import Test.Syd (Spec, sequential, sydTest)

import Analyse qualified (spec)
import Builder qualified (spec)
import Convert qualified (spec)
import StackMap qualified (spec)

spec :: Spec
spec = sequential $ do
    Convert.spec
    Builder.spec
    Analyse.spec
    StackMap.spec

main :: IO ()
main = do
    sydTest spec
