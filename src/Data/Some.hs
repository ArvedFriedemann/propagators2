module Data.Some where

import "base" Text.Show
import "base" Data.Typeable

import "hashable" Data.Hashable

import "this" Data.Typed


data Some c where
    Some :: (Typeable a, c a) => a -> Some c

some :: forall c a. (Typeable a, c a) => a -> Some c
some = Some

extractSome :: (forall a. (Typeable a, c a) => a -> b) -> Some c -> b
extractSome f (Some a) = f a

fromSome :: Typeable a => Some c -> Maybe a
fromSome = extractSome cast

mapSome :: (forall a. (Typeable a, c a) => a -> a) -> Some c -> Some c
mapSome f (Some a) = Some (f a)

class Something a
instance Something a

instance (forall a. c a => Hashable a) => Hashable (Some c) where
    hashWithSalt i (Some a) = hashWithSalt i a
    hash (Some a) = hash a
instance (forall a. c a => Eq a) => Eq (Some c) where
    Some a /= Some b = not (a =~= b)
    Some a == Some b = a =~= b
instance (forall a. c a => Ord a) => Ord (Some c) where
    Some a <  Some b = compareTyped a b == LT
    Some a <= Some b = compareTyped a b /= GT
    Some a >  Some b = compareTyped a b == GT
    Some a >= Some b = compareTyped a b /= LT
    Some a `max` Some b
        | compareTyped a b == LT = Some b
        | otherwise              = Some a
    Some a `min` Some b
        | compareTyped a b == GT = Some a
        | otherwise              = Some b
    Some a `compare` Some b = compareTyped a b
instance (forall a. c a => Show a) => Show (Some c) where
    showList = showListWith (extractSome $ showsPrec 0)
    show a = extractSome @c @ShowS (showsPrec 0) a ""
    showsPrec d (Some a)
        = showParen (d >= 10)
        $ showString "Some "
        . showsPrec 11 a
