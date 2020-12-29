module Data.Typed where

import "base" Data.Typeable


compareTyped :: (Ord a, Typeable a, Typeable b) => a -> b -> Ordering
compareTyped a b = case cast b of
    Just a' -> compare a a'
    Nothing -> typeOf a `compare` typeOf b
