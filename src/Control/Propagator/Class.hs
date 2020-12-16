{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Class
    ( PropagatorMonad(..)
    , PropagatorEqMonad(..)
    , newEmptyCell
    , linkM
    , link
    , link2
    ) where

import "base" Prelude hiding ( (.), id )
import "base" Control.Category

import "this" Data.Iso
import "this" Data.Lattice


class (Monoid (Subscription m), Monad m) => PropagatorMonad m where
    data Cell m a
    data Subscription m
    newCell :: (Meet a, Ord a) => a -> m (Cell m a)
    readCell :: (Meet a, Ord a) => Cell m a -> m a
    write :: (Meet a, Ord a) => Cell m a -> a -> m ()
    watch :: (Meet a, Ord a) => Cell m a -> (a -> m ()) -> m (Subscription m)
    cancel :: Subscription m -> m ()

class PropagatorMonad m => PropagatorEqMonad m where
    iso :: Cell m a -> Cell m b -> (a <-> b) -> m ()
    eq :: Cell m a -> Cell m a -> m ()
    eq a b = iso a b id

-- combinators

linkM :: PropagatorMonad m 
      => (Meet a, Ord a)
      => (Meet b, Ord b)
      => Cell m a -> Cell m b -> (a -> m b) -> m (Subscription m)
linkM ca cb f = watch ca $ \ a -> f a >>= write cb

link :: PropagatorMonad m 
     => (Meet a, Ord a)
     => (Meet b, Ord b)
     =>  Cell m a -> Cell m b -> (a -> b) -> m (Subscription m)
link ca cb f = linkM ca cb $ pure . f

link2 :: PropagatorMonad m 
      => (Meet a, Ord a)
      => (Meet b, Ord b)
      => (Meet c, Ord c)
      => Cell m a -> Cell m b -> Cell m c -> (a -> b -> c) -> m (Subscription m)
link2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a <$> readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b <$> readCell ca
    pure (unsubCa <> unsubCb)
    
newEmptyCell :: forall a m. (PropagatorMonad m, BoundedMeet a, Ord a) => m (Cell m a)
newEmptyCell = newCell top
