-- | Types and type synonyms for raw JVM data
module JVM.Data.Raw.Types where

import Data.Binary
import Data.Text (Text)
import Data.Bits (shiftR)

type U1 = Word8

type U2 = Word16

type U4 = Word32

type U8 = Word64

type JVMInt = U4

type JVMFloat = Float

type JVMLong = U8

type JVMDouble = Double

type JVMString = Text


-- | An offset for an instruction, used for jumps
type InstOffsetBytes = U2

-- | Converts an 'InstOffsetBytes' to a pair of bytes, encoding it a way that the JVM will correctly interpret it 
-- In other words, this function does the opposite of @(branchbyte1 << 8) | branchbyte2@
-- >>> instOffsetBytesToU2 39
-- (0,39)

instOffsetBytesToU2 :: InstOffsetBytes -> (U1, U1)
instOffsetBytesToU2 value = (fromIntegral (value `shiftR` 8), fromIntegral value)

-- | A constant pool index
type ConstantPoolIndex = U2

-- | An "array type code" for the `newarray` instruction
type ArrayType = U1
