module JVM.Data.Abstract.Builder.Code (CodeBuilderT (..), runCodeBuilderT, runCodeBuilderT', CodeBuilder, newLabel, emit, emit', runCodeBuilder, runCodeBuilder', addCodeAttribute) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.ClassFile.Method

newtype CodeBuilderT m a = CodeBuilder ( StateT CodeState (WriterT [Instruction] m) a)
    deriving (Functor, Applicative, Monad, MonadState CodeState, MonadWriter [Instruction])

unCodeBuilderT :: CodeBuilderT m a -> StateT CodeState (WriterT [Instruction] m) a
unCodeBuilderT (CodeBuilder m) = m

instance MonadTrans CodeBuilderT where
    lift = CodeBuilder . lift . lift

type CodeBuilder = CodeBuilderT Identity

data CodeState = CodeState
    { labelSource :: [Label]
    , attributes :: [CodeAttribute]
    }

initialCodeState :: CodeState
initialCodeState = CodeState{labelSource = MkLabel <$> [0 ..], attributes =[]}

newLabel :: CodeBuilder Label
newLabel = do
    s@CodeState{labelSource = ls} <- get
    case ls of
        [] -> error "No more labels"
        l : ls' -> do
            put (s{labelSource = ls'})
            pure l

emit :: Instruction -> CodeBuilder ()
emit i = tell [i]

emit' :: [Instruction] -> CodeBuilder ()
emit' = tell

addCodeAttribute :: CodeAttribute -> CodeBuilder a
addCodeAttribute ca = do
    s@CodeState{attributes = attrs} <- get
    put (s{attributes = ca : attrs})
    pure undefined

runCodeBuilder :: CodeBuilder a -> [Instruction]
runCodeBuilder = execWriter . flip evalStateT initialCodeState . unCodeBuilderT

runCodeBuilderT :: Monad m => CodeBuilderT m a -> m (a, [Instruction])
runCodeBuilderT = runWriterT . flip evalStateT initialCodeState . unCodeBuilderT

runCodeBuilder' :: CodeBuilder a -> (a, [Instruction])
runCodeBuilder' = runWriter . flip evalStateT initialCodeState . unCodeBuilderT

runCodeBuilderT' :: Monad m => CodeBuilderT m a -> m (a, [Instruction])
runCodeBuilderT' = runWriterT . flip evalStateT initialCodeState . unCodeBuilderT
