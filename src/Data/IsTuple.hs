
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Data.IsTuple where

import "base" Data.Functor.Identity
import "base" Data.Kind
import "base" GHC.TypeLits


class HasComponents a (Components a) => IsTuple a where
    type Components a :: [Type]
    fromTuple :: Tuple (Components a) -> a
    asTuple :: a -> Tuple (Components a)

type family Tuple (cx :: [Type]) = (t :: Type) | t -> cx where
    Tuple '[] = ()
    Tuple '[a] = Identity a
    Tuple '[a, b] = (a, b)
    Tuple '[a, b, c] = (a, b, c)
    Tuple '[a, b, c, d] = (a, b, c, d)
    Tuple '[a, b, c, d, e] = (a, b, c, d, e)
    Tuple '[a, b, c, d, e, f] = (a, b, c, d, e, f)
    Tuple '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
    Tuple '[a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)


pattern AsTuple :: IsTuple a => Tuple (Components a) -> a
pattern AsTuple t <- (asTuple -> t)
  where AsTuple t = fromTuple t


type HasComponents a cs = HasComponents' a cs 0
type family HasComponents' (a :: Type) (cs :: [Type]) (i :: Nat) :: Constraint where
    HasComponents' a '[] i = ()
    HasComponents' a (c ': cx) i = (HasComponent a i, HasComponents' a cx (i + 1))


class HasComponent t (i :: Nat) where
    type Component t i
    getAt :: proxy i -> t -> Component t i


instance HasComponent (Identity a) 0 where
    type Component (Identity a) 0 = a
    getAt _ = runIdentity
instance HasComponent (a, b) 0 where
    type Component (a, b) 0 = a
    getAt _ (a, _) = a
instance HasComponent (a, b, c) 0 where
    type Component (a, b, c) 0 = a
    getAt _ (a, _, _) = a
instance HasComponent (a, b, c, d) 0 where
    type Component (a, b, c, d) 0 = a
    getAt _ (a, _, _, _) = a
instance HasComponent (a, b, c, d, e) 0 where
    type Component (a, b, c, d, e) 0 = a
    getAt _ (a, _, _, _, _) = a
instance HasComponent (a, b, c, d, e, f) 0 where
    type Component (a, b, c, d, e, f) 0 = a
    getAt _ (a, _, _, _, _, _) = a
instance HasComponent (a, b, c, d, e, f, g) 0 where
    type Component (a, b, c, d, e, f, g) 0 = a
    getAt _ (a, _, _, _, _, _, _) = a
instance HasComponent (a, b, c, d, e, f, g, h) 0 where
    type Component (a, b, c, d, e, f, g, h) 0 = a
    getAt _ (a, _, _, _, _, _, _, _) = a

instance HasComponent (a, b) 1 where
    type Component (a, b) 1 = b
    getAt _ (_, b) = b
instance HasComponent (a, b, c) 1 where
    type Component (a, b, c) 1 = b
    getAt _ (_, b, _) = b
instance HasComponent (a, b, c, d) 1 where
    type Component (a, b, c, d) 1 = b
    getAt _ (_, b, _, _) = b
instance HasComponent (a, b, c, d, e) 1 where
    type Component (a, b, c, d, e) 1 = b
    getAt _ (_, b, _, _, _) = b
instance HasComponent (a, b, c, d, e, f) 1 where
    type Component (a, b, c, d, e, f) 1 = b
    getAt _ (_, b, _, _, _, _) = b
instance HasComponent (a, b, c, d, e, f, g) 1 where
    type Component (a, b, c, d, e, f, g) 1 = b
    getAt _ (_, b, _, _, _, _, _) = b
instance HasComponent (a, b, c, d, e, f, g, h) 1 where
    type Component (a, b, c, d, e, f, g, h) 1 = b
    getAt _ (_, b, _, _, _, _, _, _) = b

instance HasComponent (a, b, c) 2 where
    type Component (a, b, c) 2 = c
    getAt _ (_, _, c) = c
instance HasComponent (a, b, c, d) 2 where
    type Component (a, b, c, d) 2 = c
    getAt _ (_, _, c, _) = c
instance HasComponent (a, b, c, d, e) 2 where
    type Component (a, b, c, d, e) 2 = c
    getAt _ (_, _, c, _, _) = c
instance HasComponent (a, b, c, d, e, f) 2 where
    type Component (a, b, c, d, e, f) 2 = c
    getAt _ (_, _, c, _, _, _) = c
instance HasComponent (a, b, c, d, e, f, g) 2 where
    type Component (a, b, c, d, e, f, g) 2 = c
    getAt _ (_, _, c, _, _, _, _) = c
instance HasComponent (a, b, c, d, e, f, g, h) 2 where
    type Component (a, b, c, d, e, f, g, h) 2 = c
    getAt _ (_, _, c, _, _, _, _, _) = c

instance HasComponent (a, b, c, d) 3 where
    type Component (a, b, c, d) 3 = d
    getAt _ (_, _, _, d) = d
instance HasComponent (a, b, c, d, e) 3 where
    type Component (a, b, c, d, e) 3 = d
    getAt _ (_, _, _, d, _) = d
instance HasComponent (a, b, c, d, e, f) 3 where
    type Component (a, b, c, d, e, f) 3 = d
    getAt _ (_, _, _, d, _, _) = d
instance HasComponent (a, b, c, d, e, f, g) 3 where
    type Component (a, b, c, d, e, f, g) 3 = d
    getAt _ (_, _, _, d, _, _, _) = d
instance HasComponent (a, b, c, d, e, f, g, h) 3 where
    type Component (a, b, c, d, e, f, g, h) 3 = d
    getAt _ (_, _, _, d, _, _, _, _) = d

instance HasComponent (a, b, c, d, e) 4 where
    type Component (a, b, c, d, e) 4 = e
    getAt _ (_, _, _, _, e) = e
instance HasComponent (a, b, c, d, e, f) 4 where
    type Component (a, b, c, d, e, f) 4 = e
    getAt _ (_, _, _, _, e, _) = e
instance HasComponent (a, b, c, d, e, f, g) 4 where
    type Component (a, b, c, d, e, f, g) 4 = e
    getAt _ (_, _, _, _, e, _, _) = e
instance HasComponent (a, b, c, d, e, f, g, h) 4 where
    type Component (a, b, c, d, e, f, g, h) 4 = e
    getAt _ (_, _, _, _, e, _, _, _) = e

instance HasComponent (a, b, c, d, e, f) 5 where
    type Component (a, b, c, d, e, f) 5 = f
    getAt _ (_, _, _, _, _, f) = f
instance HasComponent (a, b, c, d, e, f, g) 5 where
    type Component (a, b, c, d, e, f, g) 5 = f
    getAt _ (_, _, _, _, _, f, _) = f
instance HasComponent (a, b, c, d, e, f, g, h) 5 where
    type Component (a, b, c, d, e, f, g, h) 5 = f
    getAt _ (_, _, _, _, _, f, _, _) = f

instance HasComponent (a, b, c, d, e, f, g) 6 where
    type Component (a, b, c, d, e, f, g) 6 = g
    getAt _ (_, _, _, _, _, _, g) = g
instance HasComponent (a, b, c, d, e, f, g, h) 6 where
    type Component (a, b, c, d, e, f, g, h) 6 = g
    getAt _ (_, _, _, _, _, _, g, _) = g

instance HasComponent (a, b, c, d, e, f, g, h) 7 where
    type Component (a, b, c, d, e, f, g, h) 7 = h
    getAt _ (_, _, _, _, _, _, _, h) = h

pattern ConsTuple :: MkTuple a ax => a -> Tuple ax -> Tuple (a ': ax)
pattern ConsTuple a ax <- (unconsTuple -> (a, ax))
    where ConsTuple a ax = consTuple a ax

class MkTuple a (ax :: [Type]) where
    unconsTuple :: Tuple (a : ax) -> (a, Tuple ax)
    unconsTuple = undefined

    consTuple :: a -> Tuple ax -> Tuple (a : ax)
    consTuple = undefined

instance MkTuple a '[] where
    unconsTuple (Identity a) = (a, ())
    consTuple a () = Identity a
instance MkTuple a '[b] where
    unconsTuple (a, b) = (a, Identity b)
    consTuple a (Identity b) = (a, b)
instance MkTuple a '[b, c] where
    unconsTuple (a, b, c) = (a, (b, c))
    consTuple a (b, c) = (a, b, c)
instance MkTuple a '[b, c, d] where
    unconsTuple (a, b, c, d) = (a, (b, c, d))
    consTuple a (b, c, d) = (a, b, c, d)
instance MkTuple a '[b, c, d, e] where
    unconsTuple (a, b, c, d, e) = (a, (b, c, d, e))
    consTuple a (b, c, d, e) = (a, b, c, d, e)
instance MkTuple a '[b, c, d, e, f] where
    unconsTuple (a, b, c, d, e, f) = (a, (b, c, d, e, f))
    consTuple a (b, c, d, e, f) = (a, b, c, d, e, f)
instance MkTuple a '[b, c, d, e, f, g] where
    unconsTuple (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))
    consTuple a (b, c, d, e, f, g) = (a, b, c, d, e, f, g)
instance MkTuple a '[b, c, d, e, f, g, h] where
    unconsTuple (a, b, c, d, e, f, g, h) = (a, (b, c, d, e, f, g, h))
    consTuple a (b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, h)
