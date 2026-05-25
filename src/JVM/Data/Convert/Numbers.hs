module JVM.Data.Convert.Numbers (toJVMFloat, toJVMLong, toJVMDouble) where

import Data.Binary.Get (getWord32be, runGet)
import Data.Binary.Put (putDoublebe, putFloatbe, runPut)
import Data.Bits (Bits (..))
import Data.Int (Int64)
import Witch (unsafeInto)

import JVM.Data.Raw.Types (U4)

-- | Converts a Haskell float to the JVM representation
toJVMFloat :: Float -> U4
toJVMFloat f = runGet getWord32be (runPut $ putFloatbe f) -- This is probably very inefficient

{- | Converts a Haskell long to the JVM representation (two U4s).
this splits the long into its high and low 32 bits.
-}
toJVMLong :: Int64 -> (U4, U4)
toJVMLong l = (unsafeInto (l `shiftR` 32), unsafeInto l)

toJVMDouble :: Double -> (U4, U4)
toJVMDouble d = do
    let bs = runPut (putDoublebe d)
    flip runGet bs $ do
        high <- getWord32be
        low <- getWord32be
        pure (high, low)
