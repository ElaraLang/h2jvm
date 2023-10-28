module JVM.Data.Abstract.Builder.Code (CodeBuilderT(..), unCodeBuilder,runCodeBuilderT, runCodeBuilderT', CodeBuilder, newLabel, emit, emit', runCodeBuilder, runCodeBuilder') where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.Instruction

newtype CodeBuilderT m a = CodeBuilder {unCodeBuilder :: StateT CodeState (WriterT [Instruction] m) a}
    deriving (Functor, Applicative, Monad, MonadState CodeState, MonadWriter [Instruction])

instance MonadTrans CodeBuilderT where
    lift = CodeBuilder . lift . lift

type CodeBuilder = CodeBuilderT Identity

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

runCodeBuilderT :: Monad m => CodeBuilderT m a -> m (a, [Instruction])
runCodeBuilderT = runWriterT . flip evalStateT initialCodeState . unCodeBuilder

runCodeBuilder' :: CodeBuilder a -> (a, [Instruction])
runCodeBuilder' = runWriter . flip evalStateT initialCodeState . unCodeBuilder

runCodeBuilderT' :: Monad m => CodeBuilderT m a -> m (a, [Instruction])
runCodeBuilderT' = runWriterT . flip evalStateT initialCodeState . unCodeBuilder
