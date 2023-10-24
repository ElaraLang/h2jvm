module JVM.Data.Abstract.Builder.Label where

newtype Label = MkLabel Int
    deriving (Show, Eq, Ord)
