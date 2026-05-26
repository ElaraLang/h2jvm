-- | Types and type synonyms for raw JVM data.
module H2JVM.Internal.Raw.Types (
    U1,
    U2,
    U4,
    U8,
    ConstantPoolIndex,
    ArrayType,
    JVMInt,
    JVMFloat,
    JVMLong,
    JVMDouble,
    JVMString,
)
where

import Data.Binary
import Data.Int (Int32, Int64)
import Data.Text (Text)

type U1 = Word8

type U2 = Word16

type U4 = Word32

type U8 = Word64

type JVMInt = Int32

type JVMFloat = Float

type JVMLong = Int64

type JVMDouble = Double

type JVMString = Text

-- | A constant pool index
type ConstantPoolIndex = U2

-- | An "array type code" for the `newarray` instruction
type ArrayType = U1
