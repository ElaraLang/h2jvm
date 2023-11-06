module JVM.Data.Abstract.Builder.Code (CodeBuilderT (..), unCodeBuilderT, runCodeBuilderT, runCodeBuilderT', CodeBuilder, newLabel, emit, emit', runCodeBuilder, runCodeBuilder', addCodeAttribute, appendStackMapFrame, getCode) where

import Control.Monad.Identity
import Control.Monad.State
import Data.TypeMergingList (TypeMergingList)
import Data.TypeMergingList qualified as TML
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.ClassFile.Method hiding (code)
import JVM.Data.Abstract.Instruction

newtype CodeBuilderT m a = CodeBuilder (StateT CodeState m a)
    deriving (Functor, Applicative, Monad, MonadState CodeState)

unCodeBuilderT :: CodeBuilderT m a -> StateT CodeState m a
unCodeBuilderT (CodeBuilder m) = m

instance MonadTrans CodeBuilderT where
    lift = CodeBuilder . lift

type CodeBuilder = CodeBuilderT Identity

data CodeState = CodeState
    { labelSource :: [Label]
    , attributes :: TypeMergingList CodeAttribute
    , code :: [Instruction]
    }

initialCodeState :: CodeState
initialCodeState = CodeState{labelSource = MkLabel <$> [0 ..], attributes = mempty, code = []}

newLabel :: CodeBuilder Label
newLabel = do
    s@CodeState{labelSource = ls} <- get
    case ls of
        [] -> error "No more labels"
        l : ls' -> do
            put (s{labelSource = ls'})
            pure l

emit :: Monad m => Instruction -> CodeBuilderT m ()
emit i = emit' [i]

emit' :: Monad m => [Instruction] -> CodeBuilderT m ()
emit' is = do
    modify (\s -> s{code = reverse is <> s.code})

addCodeAttribute :: Monad m => CodeAttribute -> CodeBuilderT m ()
addCodeAttribute ca = do
    s@CodeState{attributes = attrs} <- get
    put (s{attributes = attrs `TML.snoc` ca})
    pure ()

appendStackMapFrame :: Monad m => StackMapFrame -> CodeBuilderT m ()
appendStackMapFrame f = addCodeAttribute (StackMapTable [f])

getCode :: Monad m => CodeBuilderT m [Instruction]
getCode = gets (.code)

rr :: (a, CodeState) -> (a, [CodeAttribute], [Instruction])
rr (a, s) =
    ( a
    , TML.toList s.attributes
    , reverse s.code -- snoc list
    )



runCodeBuilder :: CodeBuilder a -> ([CodeAttribute], [Instruction])
runCodeBuilder = (\(_, b, c) -> (b, c)) . runCodeBuilder'

runCodeBuilderT :: Monad m => CodeBuilderT m a -> m (a, [CodeAttribute], [Instruction])
runCodeBuilderT = fmap rr . flip runStateT initialCodeState . unCodeBuilderT

runCodeBuilder' :: CodeBuilder a -> (a, [CodeAttribute], [Instruction])
runCodeBuilder' = rr . runIdentity . flip runStateT initialCodeState . unCodeBuilderT

runCodeBuilderT' :: Monad m => CodeBuilderT m a -> m (a, [CodeAttribute], [Instruction])
runCodeBuilderT' = fmap rr . flip runStateT initialCodeState . unCodeBuilderT
