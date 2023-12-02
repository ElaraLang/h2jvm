{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module JVM.Data.Analyse.StackMapTable.Mark where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Generics.Sum
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Traversable (for)
import Data.TypeMergingList qualified as TML
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ClassFile.Method (ClassFileMethod, CodeAttributeData (..), MethodAttribute (..), methodAttributes)
import JVM.Data.Abstract.Descriptor (MethodDescriptor)
import JVM.Data.Abstract.Instruction

data Mark = Mark
    { position :: Label
    , block :: Maybe BasicBlock
    , jump :: Maybe [Maybe BasicBlock]
    , alwaysJump :: Bool -- true if an unconditional branch
    , size :: Int -- 0 unless the mark indicates RETURN etc
    }

newMark :: Label -> Mark
newMark pos = Mark{position = pos, block = Nothing, jump = Nothing, alwaysJump = False, size = 0}

instance Eq Mark where
    a == b = a.position == b.position

instance Ord Mark where
    compare a b = compare (a.position) (b.position)

data BasicBlock = BasicBlock
    { position :: Label
    , length :: Int
    , incoming :: Int
    , exit :: Maybe [BasicBlock]
    , stop :: Bool
    }

makeBlocks :: ClassFileMethod -> Maybe [BasicBlock]
makeBlocks method = do
    codeAttr <- TML.getByCtor @"Code" method.methodAttributes

    undefined
    
makeMarks :: CodeAttributeData -> Map Label Mark
makeMarks data_ = flip execState mempty $ do
    for data_.code $ \inst -> do
        case inst of
            Goto x -> do
                to <- makeMark x
                let jumps = replicate ((fromJust $ to.block).position) Nothing
                jumps <- makeArray (to.block)
                makeMarkFor jumps

makeMark :: Label -> State (Map Label Mark) Mark
makeMark = makeMark0 True True

makeMark0 :: Bool -> Bool -> Label -> State (Map Label Mark) Mark
makeMark0 isBlockBegin isTarget label = do
    marks <- get
    let inTable = fromMaybe (newMark label) (Map.lookup label marks)
    modify $ Map.insert label inTable
    if isBlockBegin
        then do
            let newBlock = fromMaybe (makeBlock label) inTable.block
            let newBlock' = if isTarget then newBlock{incoming = newBlock.incoming + 1} else newBlock
            let newMark = inTable{block = Just newBlock'}
            modify $ Map.insert label newMark
            pure newMark
        else pure inTable

makeBlock :: Label -> BasicBlock
makeBlock label =
    BasicBlock
        { position = label
        , length = 0
        , incoming = 0
        , exit = Nothing
        , stop = False
        }