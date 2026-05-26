{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A Snoc List type that merges elements of the same constructor.
We use this for managing class attributes, where adding a new attribute of the same type should merge with the existing one rather than just being added to the list.
For example, suppose we have some data type:

@
data Entry = IntEntry Int | StringEntry String deriving (Eq, Data, Show)
instance Semigroup Entry where
   IntEntry a <> IntEntry b = IntEntry (a + b)
   StringEntry a <> StringEntry b = StringEntry (a ++ b)
   _ <> b = b -- if the constructors don't match, just take the right one
@

Then we can do:

@
snoc (TypeMergingList []) (IntEntry 1) = TypeMergingList [IntEntry 1]
snoc (TypeMergingList [IntEntry 1]) (IntEntry 2) = TypeMergingList [IntEntry 3]
snoc (TypeMergingList [IntEntry 1]) (StringEntry "hello") = TypeMergingList [IntEntry 1, StringEntry "hello"]
snoc (TypeMergingList [IntEntry 1, StringEntry "hello"]) (StringEntry "world") = TypeMergingList [IntEntry 1, StringEntry "helloworld"]
@
-}
module Data.TypeMergingList (
    -- * Data types
    TypeMergingList (..),
    DataMergeable (..),
    errorDifferentConstructors,

    -- * Operations
    snoc,

    -- * Conversions
    toList,
    toVector,
    fromList,
)
where

import Data.Data
import Data.Vector (Vector)
import GHC.Stack

import Data.Vector qualified as V
import GHC.IsList qualified as L

import JVM.Data.Pretty (Pretty (pretty))

-- | A list type that merges elements of the same constructor using the 'merge' function from the 'DataMergeable' class.
newtype TypeMergingList a = TypeMergingList [a]
    deriving (Eq, Ord, Show)

{- | Class of partially mergeable types.
Instances of this class may assume that the constructors of the two arguments are the same (i.e. @'toConstr' x == 'toConstr' y@), and
are permitted to be partial if this is not the case. The helper function 'errorDifferentConstructors' is provided for conveniently throwing an error in this case.
-}
class Data a => DataMergeable a where
    -- | Merge two values of the same constructor. This function is partial if the constructors of the two arguments are different.
    merge :: HasCallStack => a -> a -> a

-- | Convenience function for writing invalid merge errors.
errorDifferentConstructors :: (Data a, HasCallStack) => a -> a -> b
errorDifferentConstructors x y = error $ "Cannot merge values as they have different data constructors: " <> showConstr (toConstr x) <> " and " <> showConstr (toConstr y)

instance {-# OVERLAPPABLE #-} (Data a, Semigroup a) => DataMergeable a where
    merge = (<>)

{- | Append an element to the end of a 'TypeMergingList', merging it with the last element if they have the same constructor. \(O(1)\)

>>> data Entry = IntEntry Int | StringEntry String deriving (Eq, Data, Show)
>>> instance DataMergeable Entry where { IntEntry a `merge` IntEntry b = IntEntry (a + b); StringEntry a `merge` StringEntry b = StringEntry (a ++ b); _ `merge` b = b}
>>> snoc (TypeMergingList [IntEntry 1]) (IntEntry 2)
TypeMergingList [IntEntry 3]
>>> snoc (TypeMergingList [IntEntry 1]) (StringEntry "hello")
TypeMergingList [StringEntry "hello",IntEntry 1]
>>> snoc (TypeMergingList [IntEntry 1, StringEntry "hello"]) (StringEntry "world")
TypeMergingList [StringEntry "world",IntEntry 1,StringEntry "hello"]
-}
snoc :: DataMergeable a => TypeMergingList a -> a -> TypeMergingList a
snoc (TypeMergingList []) x = TypeMergingList [x]
snoc (TypeMergingList (y : ys)) x
    | toConstr y == toConstr x = TypeMergingList ((y `merge` x) : ys)
    | otherwise = TypeMergingList (x : y : ys)

{- | Append two 'TypeMergingList's, merging the last element of the first list with the first element of the second list if they have the same constructor.
\(O(M)\) where \(M\) is the length of the second list.

>>> data Entry = IntEntry Int | StringEntry String deriving (Eq, Data, Show)
>>> instance DataMergeable Entry where { IntEntry a `merge` IntEntry b = IntEntry (a + b); StringEntry a `merge` StringEntry b = StringEntry (a ++ b); _ `merge` b = b}
>>> append (TypeMergingList [IntEntry 1]) (TypeMergingList [IntEntry 2, StringEntry "hello"])
TypeMergingList [IntEntry 2,StringEntry "hello",IntEntry 1]
>>> append (TypeMergingList [IntEntry 1]) (TypeMergingList [StringEntry "hello"])
TypeMergingList [StringEntry "hello",IntEntry 1]
>>> append (TypeMergingList [IntEntry 1, StringEntry "hello"]) (TypeMergingList [StringEntry "world"])
TypeMergingList [StringEntry "world",IntEntry 1,StringEntry "hello"]
-}
append :: DataMergeable a => TypeMergingList a -> TypeMergingList a -> TypeMergingList a
append xs (TypeMergingList ys) = foldr (flip snoc) xs ys

-- | Create a 'TypeMergingList' from a regular list. \(O(N)\)
fromList :: DataMergeable a => Data a => [a] -> TypeMergingList a
fromList = foldl' snoc (TypeMergingList [])

-- | Convert a 'TypeMergingList' to a regular list. \(O(N)\)
toList :: TypeMergingList a -> [a]
toList (TypeMergingList xs) = reverse xs -- snoc list to cons list

-- | Convert a 'TypeMergingList' to a 'Vector'. \(O(N)\)
toVector :: TypeMergingList a -> Vector a
toVector (TypeMergingList xs) = V.fromList (reverse xs)

instance DataMergeable a => Semigroup (TypeMergingList a) where
    (<>) = append

instance DataMergeable a => Monoid (TypeMergingList a) where
    mempty = TypeMergingList []

instance DataMergeable a => L.IsList (TypeMergingList a) where
    type Item (TypeMergingList a) = a
    fromList = fromList
    toList = toList

instance Foldable TypeMergingList where
    -- We fold in reverse order to maintain the left-to-right order of the list, since the snoc list is stored in reverse.
    foldMap f (TypeMergingList xs) = foldMap f (reverse xs)

instance Pretty a => Pretty (TypeMergingList a) where
    pretty = foldMap pretty . toList
