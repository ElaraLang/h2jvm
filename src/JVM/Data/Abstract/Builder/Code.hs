{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module JVM.Data.Abstract.Builder.Code where

import Data.TypeMergingList (TypeMergingList)
import Data.TypeMergingList qualified as TML
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ClassFile.Method (CodeAttribute)
import JVM.Data.Abstract.Instruction
import Polysemy
import Polysemy.State

data CodeBuilder m a where
    AddCodeAttribute :: CodeAttribute -> CodeBuilder m ()
    NewLabel :: CodeBuilder m Label
    Emit' :: [Instruction] -> CodeBuilder m ()
    GetCode :: CodeBuilder m [Instruction]

makeSem ''CodeBuilder

data CodeState = CodeState
    { labelSource :: [Label]
    , attributes :: TypeMergingList CodeAttribute
    , code :: [Instruction]
    }

initialCodeState :: CodeState
initialCodeState = CodeState{labelSource = MkLabel <$> [0 ..], attributes = mempty, code = []}

-- snoc list

emit :: (Member CodeBuilder r) => Instruction -> Sem r ()
emit = emit' . pure

codeBuilderToState :: (Member (State CodeState) r) => Sem (CodeBuilder ': r) a -> Sem r a
codeBuilderToState = interpret $ \case
    AddCodeAttribute ca -> modify (\s -> s{attributes = s.attributes `TML.snoc` ca})
    NewLabel -> do
        s@CodeState{labelSource = ls} <- get
        case ls of
            [] -> error "No more labels"
            l : ls' -> do
                put (s{labelSource = ls'})
                pure l
    Emit' is -> modify (\s -> s{code = reverse is <> s.code})
    -- code is a snoc list (kind of)
    GetCode -> gets (.code)

runCodeBuilder :: forall r a. Sem (CodeBuilder ': r) a -> Sem r (a, [CodeAttribute], [Instruction])
runCodeBuilder =
    fmap rr
        . runState initialCodeState
        . codeBuilderToState
        . raiseUnder
  where
    rr (s, a) =
        ( a
        , TML.toList s.attributes
        , reverse s.code
        )
