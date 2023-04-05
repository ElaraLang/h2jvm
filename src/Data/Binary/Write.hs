{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Binary.Write where

import Data.Binary
import Data.Vector (Vector)

-- | Like 'Data.Binary.Binary' but only supporting writing
class WriteBinary a where
    writeBinary :: a -> Put

instance {-# OVERLAPPABLE #-} (Binary a) => WriteBinary a where
    writeBinary = put

writeList ::
    (WriteBinary a, Integral i, Foldable t) =>
    (i -> Put) ->
    t a ->
    Put
writeList putLength xs = putLength (fromIntegral $ length xs) *> mapM_ writeBinary xs
{-# SPECIALIZE writeList :: (WriteBinary a) => (Word16 -> Put) -> Vector a -> Put #-}
{-# SPECIALIZE writeList :: (WriteBinary a) => (Word16 -> Put) -> [a] -> Put #-}