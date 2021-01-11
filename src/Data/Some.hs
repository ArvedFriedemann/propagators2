module Data.Some where

import "base" Text.Show
import "base" Data.Typeable

import "this" Data.Typed


data Some c where
    Some :: (Typeable a, c a) => a -> Some c

extractSome :: (forall a. (Typeable a, c a) => a -> b) -> Some c -> b
extractSome f (Some a) = f a

mapSome :: (forall a. (Typeable a, c a) => a -> a) -> Some c -> Some c
mapSome f (Some a) = Some (f a)

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
