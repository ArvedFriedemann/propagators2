module Control.Propagator.Class
    ( PropagatorMonad(..)
    , newEmptyCell
    , linkM
    , linkM2
    , link
    , link2
    ) where

import "this" Data.Lattice


class Monad m => PropagatorMonad m where
    data Cell m a
    newCell :: (Meet a, Ord a) => a -> m (Cell m a)
    readCell :: (Meet a, Ord a) => Cell m a -> m a
    write :: (Meet a, Ord a) => Cell m a -> a -> m ()
    watch :: (Meet a, Ord a) => Cell m a -> (a -> m ()) -> m (m ())

-- combinators

linkM :: PropagatorMonad m
      => (Meet a, Ord a)
      => (Meet b, Ord b)
      => Cell m a -> Cell m b -> (a -> m b) -> m (m ())
linkM ca cb f = watch ca $ \ a -> f a >>= write cb

linkM2 :: PropagatorMonad m
      => (Meet a, Ord a)
      => (Meet b, Ord b)
      => (Meet c, Ord c)
      => Cell m a -> Cell m b -> Cell m c -> (a -> b -> m c) -> m (m ())
linkM2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a =<< readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b =<< readCell ca
    pure (unsubCa >> unsubCb)

link :: PropagatorMonad m
     => (Meet a, Ord a)
     => (Meet b, Ord b)
     =>  Cell m a -> Cell m b -> (a -> b) -> m (m ())
link ca cb f = linkM ca cb $ pure . f

link2 :: PropagatorMonad m
      => (Meet a, Ord a)
      => (Meet b, Ord b)
      => (Meet c, Ord c)
      => Cell m a -> Cell m b -> Cell m c -> (a -> b -> c) -> m (m ())
link2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a <$> readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b <$> readCell ca
    pure (unsubCa >> unsubCb)

newEmptyCell :: forall a m. (PropagatorMonad m, BoundedMeet a, Ord a) => m (Cell m a)
newEmptyCell = newCell top
