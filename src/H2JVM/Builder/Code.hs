{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A monadic builder for JVM bytecode instructions.
module H2JVM.Builder.Code (
    CodeBuilder,
    emit,
    addCodeAttribute,
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

-- | The 'CodeBuilder' effect, which allows emitting instructions and adding code attributes in a monadic style.
data CodeBuilder m a where
    -- | Add a code attribute to the current code block. If an attribute of the same type already exists, it will be merged with the existing one using the 'H2JVM.Data.TypeMergingList.merge' function from the 'H2JVM.Data.TypeMergingList.DataMergeable' instance for that attribute type.
    AddCodeAttribute ::
        -- | the code attribute to add
        CodeAttribute ->
        CodeBuilder m ()
    -- | Create a new, unique 'Label'.
    NewLabel ::
        CodeBuilder m Label
    Emit' :: [Instruction] -> CodeBuilder m ()

makeEffect ''CodeBuilder

data CodeState = CodeState
    { currentLabel :: {-# UNPACK #-} !Word
    -- ^ the current label index.
    , attributes :: TypeMergingList CodeAttribute
    -- ^ the code attributes that have been added to this code block.
    , code :: [Instruction]
    -- ^ the code that has been emitted so far. Note that this is stored in reverse order for efficient appending, so it should be reversed before being returned.
    }

-- | An empty code state, with an infinite supply of labels.
initialCodeState :: CodeState
initialCodeState = CodeState{currentLabel = 0, attributes = mempty, code = []}

-- | Emit a single instruction.
emit :: CodeBuilder :> r => Instruction -> Eff r ()
emit = emit' . pure @[]

-- | Re-interpret a 'CodeBuilder' effect as a 'State' effect, accumulating the emitted code and attributes in the state.
codeBuilderToState :: Eff (CodeBuilder : es) a -> Eff es (a, CodeState)
codeBuilderToState =
    reinterpret
        (runState initialCodeState)
        ( \_ -> \case
            AddCodeAttribute ca -> modify $ \s -> s{attributes = s.attributes `TML.snoc` ca}
            NewLabel -> do
                c <- gets (.currentLabel)
                modify $ \s -> s{currentLabel = c + 1}
                pure (unsafeMkLabel c)
            Emit' is -> modify $ \s -> s{code = reverse is <> s.code}
        )

{- | Run a 'CodeBuilder' effect, returning the emitted code and attributes.
Throws an imprecise exception if no code was emitted, since a code block with no instructions is invalid.
-}
runCodeBuilder ::
    forall r a.
    HasCallStack =>
    Eff (CodeBuilder ': r) a ->
    -- | the result of the computation, the list of emitted code attributes, and the list of emitted instructions, in the order they were emitted.
    Eff r (a, [CodeAttribute], NonEmpty Instruction)
runCodeBuilder =
    fmap formatResult . codeBuilderToState
  where
    formatResult (res, s) =
        ( res
        , TML.toList s.attributes
        , case reverse s.code of
            [] -> error "runCodeBuilder: No code emitted"
            is -> NE.fromList is
        )
