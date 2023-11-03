module JVM.Data.Abstract.Builder.Code (CodeBuilderT (..), unCodeBuilderT, runCodeBuilderT, runCodeBuilderT', CodeBuilder, newLabel, emit, emit', runCodeBuilder, runCodeBuilder', addCodeAttribute, appendStackMapFrame) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.TypeMergingList (TypeMergingList)
import Data.TypeMergingList qualified as TML
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Instruction
import JVM.Data.Raw.ClassFile (Attribute (CodeAttribute))

newtype CodeBuilderT m a = CodeBuilder (StateT CodeState (WriterT [Instruction] m) a)
    deriving (Functor, Applicative, Monad, MonadState CodeState, MonadWriter [Instruction])

unCodeBuilderT :: CodeBuilderT m a -> StateT CodeState (WriterT [Instruction] m) a
unCodeBuilderT (CodeBuilder m) = m

instance MonadTrans CodeBuilderT where
    lift = CodeBuilder . lift . lift

type CodeBuilder = CodeBuilderT Identity

data CodeState = CodeState
    { labelSource :: [Label]
    , attributes :: TypeMergingList CodeAttribute
    }

initialCodeState :: CodeState
initialCodeState = CodeState{labelSource = MkLabel <$> [0 ..], attributes = mempty}

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

addCodeAttribute :: CodeAttribute -> CodeBuilder ()
addCodeAttribute ca = do
    s@CodeState{attributes = attrs} <- get
    put (s{attributes = attrs `TML.snoc` ca})
    pure ()

appendStackMapFrame :: StackMapFrame -> CodeBuilder ()
appendStackMapFrame f = addCodeAttribute (StackMapTable [f])

rr :: ((a, CodeState), [Instruction]) -> (a, [CodeAttribute], [Instruction])
rr ((a, s), is) = (a, TML.toList s.attributes, is)

runCodeBuilder :: CodeBuilder a -> ([CodeAttribute], [Instruction])
runCodeBuilder = (\(_, b, c) -> (b, c)) . runCodeBuilder'

runCodeBuilderT :: Monad m => CodeBuilderT m a -> m (a, [CodeAttribute], [Instruction])
runCodeBuilderT = fmap rr . runWriterT . flip runStateT initialCodeState . unCodeBuilderT

runCodeBuilder' :: CodeBuilder a -> (a, [CodeAttribute], [Instruction])
runCodeBuilder' = rr . runWriter . flip runStateT initialCodeState . unCodeBuilderT

runCodeBuilderT' :: Monad m => CodeBuilderT m a -> m (a, [CodeAttribute], [Instruction])
runCodeBuilderT' = fmap rr . runWriterT . flip runStateT initialCodeState . unCodeBuilderT
