module Data.Typed where

import "base" Data.Typeable


(=~=) :: (Eq a, Typeable a, Typeable b) => a -> b -> Bool
a =~= b = pure a == cast b

compareTyped :: (Ord a, Typeable a, Typeable b) => a -> b -> Ordering
compareTyped a b = case cast b of
    Just a' -> compare a a'
    Nothing -> typeOf a `compare` typeOf b
