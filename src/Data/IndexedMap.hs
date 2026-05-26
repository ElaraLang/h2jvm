{-# LANGUAGE PartialTypeSignatures #-}

{- | An indexed map is an efficient map with integer keys, that can efficiently retrieve the key from a value.
This is used to efficiently build up a constant pool without duplicating entries, since the constant pool is indexed by integers, and we often need to check if a value is already in the constant pool before inserting it.
Because of the specialised nature, its indexes start at 1, not 0. I would apologise but I'm not sorry.
-}
module Data.IndexedMap (
    -- * Types
    IndexedMap,
    Index,
    indexValue,

    -- * Construction
    empty,
    singleton,

    -- * Lookup
    lookup,
    lookupIndex,
    lookupIndexWhere,
    isEmpty,

    -- * Insertion
    insert,
    lookupOrInsert,
    lookupOrInsertM,
    lookupOrInsertMOver,

    -- * Conversion
    toVector,
)
where

import Control.Lens (Lens', set, view)
import Control.Monad (forM_)
import Data.Vector (Vector)
import Data.Word (Word32)
import Effectful
import Effectful.State.Static.Local
import GHC.Exts (IsList (..))
import Prelude hiding (lookup)

import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Vector qualified as V

-- | An index into the map.
newtype Index = Index Word32 deriving (Eq, Num, Ord, Show)

-- | Get the raw value of an index.
indexValue :: Index -> Word32
indexValue (Index i) = i

-- | An indexed map is a map from integer indexes to values, with a reverse map from values to indexes.
data IndexedMap a = IndexedMap !(IM.IntMap a) !(M.Map a Int)

{- | An empty indexed map.

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
singleton :: Ord a => a -> IndexedMap a
singleton a = IndexedMap (IM.singleton 1 a) (M.singleton a 1)

{- | Lookup a value in the map by its index.

>>> lookup @String 1 (singleton "hello")
Just "hello"
-}
lookup :: Index -> IndexedMap a -> Maybe a
lookup (Index i) (IndexedMap m _) = IM.lookup (fromIntegral i) m

{- | Lookup a value in the map, returning its index if it exists.

>>> lookupIndex @String "hello" (singleton "hello")
Just 1
>>> lookupIndex @String "hello" (singleton "world")
Nothing
>>> lookupIndex @String "hello" (singleton "world" <> singleton "hello")
Just 2
-}
lookupIndex :: Ord a => a -> IndexedMap a -> Maybe Index
lookupIndex a (IndexedMap _ m) = Index . fromIntegral <$> M.lookup a m

{- | Find the index of the first element that satisfies the predicate, if any.

>>> lookupIndexWhere (== "hello") (singleton "hello")
Just 1
>>> lookupIndexWhere (== "hello") (singleton "world")
Nothing
-}
lookupIndexWhere :: (a -> Bool) -> IndexedMap a -> Maybe Index
lookupIndexWhere f (IndexedMap m _) = Index . fromIntegral . fst <$> IM.lookupMin (IM.filter f m)

{- | Insert a value into the map without checking if it already exists.
In other words, this will insert a duplicate value if it already exists in the map, and return a new index for it.

>>> insert "hello" empty
(Index 1,fromList [(1,"hello")])

>>> insert "world" (singleton "hello")
(Index 2,fromList [(1,"hello"),(2,"world")])
>>> insert "hello" (singleton "hello")
(Index 2,fromList [(1,"hello"),(2,"hello")])
-}
insert :: Ord a => a -> IndexedMap a -> (Index, IndexedMap a)
insert a (IndexedMap m m') = (Index $ fromIntegral i, IndexedMap (IM.insert i a m) (M.insert a i m'))
  where
    i = 1 + IM.size m

{- | Lookup a value in the map, or insert it if it doesn't exist.
If the value already exists, this will return the existing index and the original map.

>>> lookupOrInsert "hello" (singleton "hello")
(Index 1,fromList [(1,"hello")])
>>> lookupOrInsert "world" (singleton "hello")
(Index 2,fromList [(1,"hello"),(2,"world")])
-}
lookupOrInsert :: Ord a => a -> IndexedMap a -> (Index, IndexedMap a)
lookupOrInsert a (IndexedMap m m') = case M.lookup a m' of
    Just i -> (Index $ fromIntegral i, IndexedMap m m')
    Nothing -> insert a (IndexedMap m m')

-- | A monadic version of 'lookupOrInsert' that can be used in a state monad.
lookupOrInsertM :: (State (IndexedMap a) :> r, Ord a) => a -> Eff r Index
lookupOrInsertM = lookupOrInsertMOver id

{- | A more general version of 'lookupOrInsertM' that allows you to specify which lens to use for the state.
This is useful if you have an 'IndexedMap' within a larger state, and you want to avoid having to manually get and put the 'IndexedMap' every time you want to lookup or insert a value.
-}
lookupOrInsertMOver :: (State a :> r, Ord b) => Lens' a (IndexedMap b) -> b -> Eff r Index
lookupOrInsertMOver lens a = do
    i <- gets (view lens)
    let (idx, new) = lookupOrInsert a i
    modify (set lens new)
    pure idx

{- | Check if the map is empty

>>> isEmpty empty
True
>>> isEmpty (singleton "hello")
False
-}
isEmpty :: IndexedMap a -> Bool
isEmpty (IndexedMap m _) = IM.null m

{- | \(O(n)\) conversion to a 'Vector'. Duplicates are not removed, and the order of the vector is the order of the indexes in the map.
This relies on the fact that 'IndexedMap' is strictly increasing in the key, so we can just generate a vector of the appropriate length and fill it with the values from the map.

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

--
-- Instances
--

instance Show a => Show (IndexedMap a) where
    show (IndexedMap im _) = show im

instance Eq a => Eq (IndexedMap a) where
    (IndexedMap im _) == (IndexedMap im' _) = im == im'

instance Ord a => Ord (IndexedMap a) where
    compare (IndexedMap im _) (IndexedMap im' _) = compare im im'

instance Foldable IndexedMap where
    foldMap f (IndexedMap im _) = foldMap f im

instance Ord a => IsList (IndexedMap a) where
    type Item (IndexedMap a) = a
    fromList = foldr (\a b -> snd $ insert a b) empty
    toList = toList . toVector

-- | Left-biased union of the two maps.
instance Ord a => Semigroup (IndexedMap a) where
    l <> r =
        runPureEff $ execState empty $ do
            forM_ (toVector l) lookupOrInsertM
            forM_ (toVector r) lookupOrInsertM

-- | Monoid instance for 'IndexedMap'.
instance Ord a => Monoid (IndexedMap a) where
    mempty = empty
