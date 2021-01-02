module Control.Propagator.StateProp where

import "base" Control.Applicative

import "this" Data.Lattice


class MonadNewVal m v where
  newVal :: a -> m (v a)
  newEmptyVal :: (BoundedMeet a) => m (v a)
  newEmptyVal = newVal top

class MonadReadVal m v where
  readVal :: v a -> m a

class MonadWriteVal m v where
  writeVal :: (Meet a) => v a -> a -> m ()

--Problem here: how to create the lense lazily?
--e.g.: only create values once they are read. This changes the morphism depending on the executon order!
--solution: stateful morphisms.
class (MonadReadVal m v) => MonadReadLense m v where
  readValLense :: (v a -> v b) -> v a -> m b
  readValLense trans r = readVal (trans r)

--TODO: Have stateful functions like functions. So when a function can be executed at any time, but it only gives the result of its first query, check whether the value already exists and if not create it.
