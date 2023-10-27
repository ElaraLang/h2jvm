module JVM.Data.Abstract.Builder.Code (CodeBuilder, newLabel, emit, emit', runCodeBuilder) where

import Control.Monad.State
import Control.Monad.Writer
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.Instruction

newtype CodeBuilder a = CodeBuilder {unCodeBuilder :: StateT CodeState (Writer [Instruction]) a}
    deriving (Functor, Applicative, Monad, MonadState CodeState, MonadWriter [Instruction])

newtype CodeState = CodeState
    { labelSource :: [Label]
    }

initialCodeState :: CodeState
initialCodeState = CodeState{labelSource = MkLabel <$> [0 ..]}

newLabel :: CodeBuilder Label
newLabel = do
    CodeState{labelSource = ls} <- get
    case ls of
        [] -> error "No more labels"
        l : ls' -> do
            put (CodeState{labelSource = ls'})
            pure l

emit :: Instruction -> CodeBuilder ()
emit i = tell [i]

emit' :: [Instruction] -> CodeBuilder ()
emit' = tell

runCodeBuilder :: CodeBuilder a -> [Instruction]
runCodeBuilder = execWriter . flip evalStateT initialCodeState . unCodeBuilder