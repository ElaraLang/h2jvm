{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for writing binary data.
module H2JVM.Internal.Binary.Write (WriteBinary (..), writeList) where

import Data.Binary
import Data.Vector (Vector)
import GHC.Stack
import Witch

-- | Like 'Data.Binary.Binary' but only supporting writing
class WriteBinary a where
    writeBinary :: a -> Put

instance {-# OVERLAPPABLE #-} Binary a => WriteBinary a where
    writeBinary = put

{- | Write a list of items, prefixed by its length.
If the length of the list cannot be safely converted to the type of length prefix, this will throw an error.
-}
writeList ::
    (WriteBinary a, Integral i, Foldable t, HasCallStack, TryFrom Int i) =>
    -- | function to write the length of the list
    (i -> Put) ->
    t a ->
    Put
writeList putLength xs =
    let len' = tryInto (length xs)
     in case len' of
            Left _ -> error $ "Cannot write list of length " <> show (length xs) <> " as it cannot be safely converted to the length prefix type."
            Right len -> putLength len *> mapM_ writeBinary xs
{-# SPECIALIZE writeList :: WriteBinary a => (Word16 -> Put) -> Vector a -> Put #-}
{-# SPECIALIZE writeList :: WriteBinary a => (Word16 -> Put) -> [a] -> Put #-}
