{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A monadic builder for JVM bytecode instructions.
module H2JVM.Builder.Code (
    CodeBuilder,
    emit,
    runCodeBuilder,
    newLabel,
)
where

import Data.List.NonEmpty (NonEmpty)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH (makeEffect)

import Data.List.NonEmpty qualified as NE

import H2JVM.Builder.Label
import H2JVM.ClassFile.Method (CodeAttribute)
import H2JVM.Data.TypeMergingList (TypeMergingList)
import H2JVM.Instruction

import H2JVM.Data.TypeMergingList qualified as TML

data CodeBuilder m a where
    AddCodeAttribute :: CodeAttribute -> CodeBuilder m ()
    NewLabel :: CodeBuilder m Label
    Emit' :: [Instruction] -> CodeBuilder m ()
    GetCode :: CodeBuilder m [Instruction]

makeEffect ''CodeBuilder

data CodeState = CodeState
    { labelSource :: [Label]
    , attributes :: TypeMergingList CodeAttribute
    , code :: [Instruction]
    }

initialCodeState :: CodeState
initialCodeState = CodeState{labelSource = unsafeMkLabel <$> [0 ..], attributes = mempty, code = []}

emit :: CodeBuilder :> r => Instruction -> Eff r ()
emit = emit' . pure

codeBuilderToState :: State CodeState :> r => Eff (CodeBuilder ': r) a -> Eff r a
codeBuilderToState = interpret $ \_ -> \case
    AddCodeAttribute ca -> modify (\s -> s{attributes = s.attributes `TML.snoc` ca})
    NewLabel -> do
        s@CodeState{labelSource = ls} <- get
        case ls of
            [] -> error "No more labels"
            l : ls' -> do
                put (s{labelSource = ls'})
                pure l
    Emit' is -> modify (\s -> s{code = reverse is <> s.code})
    GetCode -> gets (.code)

runCodeBuilder :: forall r a. HasCallStack => Eff (CodeBuilder ': r) a -> Eff r (a, [CodeAttribute], NonEmpty Instruction)
runCodeBuilder =
    fmap rr
        . runState initialCodeState
        . codeBuilderToState
        . inject
  where
    rr (a, s) =
        ( a
        , TML.toList s.attributes
        , case reverse s.code of
            [] -> error "runCodeBuilder: No code emitted"
            is -> NE.fromList is
        )
