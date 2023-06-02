-- | Types and type synonyms for raw JVM data
module JVM.Data.Raw.Types where

import Data.Binary
import Data.Text (Text)

type U1 = Word8

type U2 = Word16

type U4 = Word32

type U8 = Word64

type JVMInt = U4

type JVMFloat = Float

type JVMLong = U8

type JVMDouble = Double

type JVMString = Text

-- | A constant pool index
type ConstantPoolIndex = U2

-- | An "array type code" for the `newarray` instruction
type ArrayType = U1
