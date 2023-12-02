{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A Snoc List type that merges elements of the same constructor using the Semigroup instance.
For example, suppose we have some data type:
> data Entry = IntEntry Int | StringEntry String deriving (Eq, Data, Show)
> instance Semigroup Entry where
>     IntEntry a <> IntEntry b = IntEntry (a + b)
>     StringEntry a <> StringEntry b = StringEntry (a ++ b)
>     _ <> b = b -- if the constructors don't match, just take the right one

Then we can do:
> snoc (TypeMergingList []) (IntEntry 1) = TypeMergingList [IntEntry 1]
> snoc (TypeMergingList [IntEntry 1]) (IntEntry 2) = TypeMergingList [IntEntry 3]
> snoc (TypeMergingList [IntEntry 1]) (StringEntry "hello") = TypeMergingList [IntEntry 1, StringEntry "hello"]
> snoc (TypeMergingList [IntEntry 1, StringEntry "hello"]) (StringEntry "world") = TypeMergingList [IntEntry 1, StringEntry "helloworld"]
-}
module Data.TypeMergingList where

import Control.Lens ((^?))
import Data.Data
import Data.Generics.Sum.Constructors
import Data.List (foldl')
import GHC.Generics (Generic)
import GHC.IsList qualified as L
import Data.Vector (Vector)
import Data.Vector qualified as V

newtype TypeMergingList a = TypeMergingList [a]
    deriving (Eq, Ord, Show)

{- | Class of partially mergeable types.
Instances of this class may assume that the constructors of the two arguments are the same (i.e. @toConstr x == toConstr y@), and
are permitted to be partial if this is not the case.
-}
class Data a => DataMergeable a where
    merge :: a -> a -> a

errorDifferentConstructors :: Data a => a -> a -> b
errorDifferentConstructors x y = error $ "Cannot merge values as they have different data constructors: " <> showConstr (toConstr x) <> " and " <> showConstr (toConstr y)

instance {-# OVERLAPPABLE #-} (Data a, Semigroup a) => DataMergeable a where
    merge = (<>)

getByCtor :: forall ctor s a. (Generic s, AsConstructor ctor s s a a) => TypeMergingList s -> Maybe a
getByCtor (TypeMergingList xs) = go xs
  where
    go [] = Nothing
    go (x : xs') = case x ^? _Ctor @ctor of
        Just a -> Just a
        Nothing -> go xs'

snoc :: (DataMergeable a) => TypeMergingList a -> a -> TypeMergingList a
snoc xs x = append xs (TypeMergingList [x])

append :: (DataMergeable a) => TypeMergingList a -> TypeMergingList a -> TypeMergingList a
append (TypeMergingList xs) (TypeMergingList ys) = TypeMergingList (go xs ys)
  where
    go :: (Data a, DataMergeable a) => [a] -> [a] -> [a]
    go [] ys' = ys'
    go xs' [] = xs'
    go (x : xs') (y : ys')
        | toConstr x == toConstr y = (x `merge` y) : go xs' ys'
        | otherwise = y : go (x : xs') ys'

fromList :: DataMergeable a => Data a => [a] -> TypeMergingList a
fromList = foldl' snoc (TypeMergingList [])

toList :: TypeMergingList a -> [a]
toList (TypeMergingList xs) = reverse xs -- snoc list to cons list

toVector :: TypeMergingList a -> Vector a
toVector (TypeMergingList xs) = V.fromList (reverse xs)

instance (DataMergeable a) => Semigroup (TypeMergingList a) where
    (<>) = append

instance (DataMergeable a) => Monoid (TypeMergingList a) where
    mempty = TypeMergingList []

instance (DataMergeable a) => L.IsList (TypeMergingList a) where
    type Item (TypeMergingList a) = a
    fromList = fromList
    toList = toList


instance Foldable TypeMergingList where
    foldMap f (TypeMergingList xs) = foldMap f xs
