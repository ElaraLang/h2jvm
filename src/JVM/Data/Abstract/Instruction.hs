{-# LANGUAGE DataKinds #-}

-- | High level representation of a JVM instruction, with type-safe arguments and no stack manipulation needed.
module JVM.Data.Abstract.Instruction where

import Control.Monad.Free hiding (Free, Pure)
import Control.Monad.Free.Church (foldF)
import Control.Monad.Trans.Free
import JVM.Data.Abstract.ConstantPool (ConstantPoolEntry (CPIntegerEntry), ConstantPoolM, findIndexOf)
import JVM.Data.Raw.Instruction qualified as Raw

type Reference = Int

data Instruction
    = -- | Load reference from array
      AALoad
        Reference
        -- ^ The array to load from
        Int
        -- ^ The index to load
    | -- | Store reference in array
      AAStore Reference Int
    deriving (Show, Eq, Ord)

-- | The return type of an instruction, i.e the type of the values at the top of the stack after the instruction is executed.
type family InstructionRet instruction where
    InstructionRet ('AALoad _ _) = Reference
    InstructionRet ('AAStore _ _) = ()
    InstructionRet _ = ()

-- | Free monad for JVM instructions, with each instruction being a different constructor.
data InstructionF next where
    AALoadF :: Reference -> Int -> (Reference -> next) -> InstructionF next
    AAStoreF :: Reference -> Int -> () -> next -> InstructionF next

deriving instance Functor InstructionF

-- | Instruction monad, which is a free monad over the InstructionF functor.
type InstructionM = Free InstructionF

-- | Lift an instruction into the instruction monad.
liftInstruction :: InstructionF a -> InstructionM a
liftInstruction = liftF

aaLoad :: Reference -> Int -> InstructionM Reference
aaLoad ref index = liftInstruction (AALoadF ref index id)

aaStore :: Reference -> Int -> InstructionM ()
aaStore ref index = liftInstruction (AAStoreF ref index () ())

{- | Run an instruction monad, returning the instruction list
 >>> runInstructionM (aaLoad 0 1 >>= aaStore 2)
 [AALoad 0 1,AAStore 2 0]
-}
runInstructionM :: InstructionM a -> ConstantPoolM [Raw.Instruction]
runInstructionM program = case runFree program of
    Pure _ -> pure []
    Free (AALoadF ref index next) -> do
        ref' <- findIndexOf (CPIntegerEntry (fromIntegral ref))
        index' <- findIndexOf (CPIntegerEntry (fromIntegral index))
        pure
            [ Raw.LDC ref'
            , Raw.LDC index'
            , Raw.AALoad
            ] <> runInstructionM (next 0)
    Free (AAStoreF ref index () next) -> do
        ref' <- findIndexOf (CPIntegerEntry (fromIntegral ref))
        index' <- findIndexOf (CPIntegerEntry (fromIntegral index))
        pure
            [ Raw.LDC ref'
            , Raw.LDC index'
            , Raw.AAStore
            ]
            <> runInstructionM next