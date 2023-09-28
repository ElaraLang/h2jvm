{-# LANGUAGE PartialTypeSignatures #-}

{- | An indexed map is an efficient map with integer keys, that can efficiently retrieve the key from a value.
This is used to efficiently build up a constant pool without duplicating entries.
Because of the specialised nature, its indexes start at 1, not 0. I would apologise but I'm not sorry.
-}
module Data.IndexedMap where

import Control.Lens (Lens', assign, use)
import Control.Monad (forM_)
import Control.Monad.State (MonadState, execState)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Vector (Vector)
import Data.Vector qualified as V
import Prelude hiding (lookup)

data IndexedMap a = IndexedMap !(IM.IntMap a) !(M.Map a Int)

instance (Show a) => Show (IndexedMap a) where
    show (IndexedMap im _) = show im

instance (Eq a) => Eq (IndexedMap a) where
    (IndexedMap im _) == (IndexedMap im' _) = im == im'

instance (Ord a) => Ord (IndexedMap a) where
    compare (IndexedMap im _) (IndexedMap im' _) = compare im im'

{- | An empty indexed map
>>> lookup @String 1 empty
Nothing
-}
empty :: IndexedMap a
empty = IndexedMap IM.empty M.empty

{- | Create an indexed map with a single element
>>> lookup @String 1 (singleton "hello")
Just "hello"
>>> lookup @String 2 (singleton "hello")
Nothing
-}
singleton :: (Ord a) => a -> IndexedMap a
singleton a = IndexedMap (IM.singleton 1 a) (M.singleton a 1)

lookup :: Int -> IndexedMap a -> Maybe a
lookup i (IndexedMap m _) = IM.lookup i m

{- | Lookup a value in the map
>>> lookupIndex @String "hello" (singleton "hello")
Just 1
>>> lookupIndex @String "hello" (singleton "world")
Nothing
>>> lookupIndex @String "hello" (singleton "world" <> singleton "hello")
Just 2
-}
lookupIndex :: (Ord a) => a -> IndexedMap a -> Maybe Int
lookupIndex a (IndexedMap _ m) = M.lookup a m

-- | Insert a value into the map without checking if it already exists
insert :: (Ord a) => a -> IndexedMap a -> (Int, IndexedMap a)
insert a (IndexedMap m m') = (i, IndexedMap (IM.insert i a m) (M.insert a i m'))
  where
    i = 1 + IM.size m

-- | Lookup a value in the map, or insert it if it doesn't exist
lookupOrInsert :: (Ord a) => a -> IndexedMap a -> (Int, IndexedMap a)
lookupOrInsert a (IndexedMap m m') = case M.lookup a m' of
    Just i -> (i, IndexedMap m m')
    Nothing -> insert a (IndexedMap m m')

lookupOrInsertM :: (Ord a, MonadState (IndexedMap a) m) => a -> m Int
lookupOrInsertM = lookupOrInsertMOver id

lookupOrInsertMOver :: (MonadState a m, Ord b) => Lens' a (IndexedMap b) -> b -> m Int
lookupOrInsertMOver lens a = do
    i <- use lens
    let (idx, new) = lookupOrInsert a i
    assign lens new
    pure idx

isEmpty :: IndexedMap a -> Bool
isEmpty (IndexedMap m _) = IM.null m

{- | O(n) conversion to a vector
This relies on the fact that IndexedMap is strictly increasing in the key

>>> toVector (singleton @Int 1)
[1]

>>> toVector (singleton @Int 1 <> singleton 2)
[1,2]

>>> toVector (singleton @Int 1 <> singleton 2 <> singleton 1)
[1,2]
-}
toVector :: IndexedMap a -> Vector a
toVector i | isEmpty i = V.empty
toVector (IndexedMap im _) = do
    let (maxIndex, _) = IM.findMax im
    V.generate maxIndex ((im IM.!) . (1 +))

{- | Semigroup instance for IndexedMap
 | This is a left-biased union of the two maps
-}
instance (Ord a) => Semigroup (IndexedMap a) where
    l <> r =
        flip execState empty $ do
            forM_ (toVector l) lookupOrInsertM
            forM_ (toVector r) lookupOrInsertM

-- | Monoid instance for IndexedMap
instance (Ord a) => Monoid (IndexedMap a) where
    mempty = empty
