{-# LANGUAGE DataKinds #-}

{- | Generate a stack map table for a method.
This process MUST run last in the high level stage -
modifications to the code after this point will invalidate the stack map table and cause invalid class files to be generated.
-}
module JVM.Data.Analyse.StackMap where

import Control.Lens.Extras (is)
import Data.Generics.Sum (AsAny (_As))
import Data.List.Split (split, splitOn, splitWhen)
import JVM.Data.Abstract.Instruction

data BasicBlock = BasicBlock
    { index :: Int
    , instructions :: [Instruction]
    }
    deriving (Show, Eq)

splitIntoBasicBlocks :: [Instruction] -> [BasicBlock]
splitIntoBasicBlocks [] = []
splitIntoBasicBlocks l =
    let blocks = splitWhen (is (_As @"Label")) l
     in zipWith BasicBlock [0 ..] blocks
