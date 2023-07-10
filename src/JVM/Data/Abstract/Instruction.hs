{-# LANGUAGE DataKinds #-}

-- | High level representation of a JVM instruction, with type-safe arguments and no stack manipulation needed.
module JVM.Data.Abstract.Instruction where

import Data.Text (Text)
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Type (ClassInfoType)

type Reference = Int

data Instruction
    = ALoad0
    | InvokeStatic ClassInfoType Text MethodDescriptor
    | AReturn
    deriving (Show, Eq, Ord)
