{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Class
    ( PropagatorMonad(..)
    , PropagatorEqMonad(..)
    , newEmptyCell
    , linkM
    , linkM2
    , link
    , link2
    , eqAll
    ) where

import "base" Prelude hiding ( (.), id )
import "base" Control.Category

import "this" Data.Iso
import "this" Data.Lattice


class ( forall a. Ord (Cell m a)
      , forall a. Show (Cell m a)
      , Ord (Subscription m)
      , Show (Subscription m)
      , Monoid (Subscription m)
      , Monad m
      ) => PropagatorMonad m where

    data Cell m :: * -> *
    data Subscription m
    newCell :: (Meet a, Ord a) => String -> a -> m (Cell m a)
    newCell' :: (Meet a, Ord a) => a -> m (Cell m a)
    newCell' = newCell ""
    readCell :: (Meet a, Ord a) => Cell m a -> m a
    write :: (Meet a, Ord a) => Cell m a -> a -> m ()
    watch :: (Meet a, Ord a) => Cell m a -> (a -> m ()) -> m (Subscription m)
    cancel :: Subscription m -> m ()


class PropagatorMonad m => PropagatorEqMonad m where
    iso :: (Meet a, Ord a, Meet b, Ord b) => Cell m a -> Cell m b -> (a <-> b) -> m ()
    eq :: (Meet a, Ord a) => Cell m a -> Cell m a -> m ()
    eq a b = iso a b id

eqAll :: (PropagatorEqMonad m, Meet a, Ord a) => [Cell m a] -> m ()
eqAll [] = pure ()
eqAll (a : as) = mapM_ (eq a) as

-------------------------------------------------------------------------------
-- combinators
-------------------------------------------------------------------------------

linkM :: ( PropagatorMonad m
         , Meet a, Ord a
         , Meet b, Ord b
         )
      => Cell m a -> Cell m b -> (a -> m b) -> m (Subscription m)
linkM ca cb f = watch ca $ \ a -> f a >>= write cb

linkM2 :: ( PropagatorMonad m
          , Meet a, Ord a
          , Meet b, Ord b
          , Meet c, Ord c
          )
      => Cell m a -> Cell m b -> Cell m c -> (a -> b -> m c) -> m (Subscription m)
linkM2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a =<< readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b =<< readCell ca
    pure (unsubCa <> unsubCb)

link :: ( PropagatorMonad m
        , Meet a, Ord a
        , Meet b, Ord b
        )
     =>  Cell m a -> Cell m b -> (a -> b) -> m (Subscription m)
link ca cb f = linkM ca cb $ pure . f

link2 :: ( PropagatorMonad m
         , Meet a, Ord a
         , Meet b, Ord b
         , Meet c, Ord c
         )
      => Cell m a -> Cell m b -> Cell m c -> (a -> b -> c) -> m (Subscription m)
link2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a <$> readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b <$> readCell ca
    pure (unsubCa <> unsubCb)

newEmptyCell :: forall a m. (PropagatorMonad m, BoundedMeet a, Ord a) => String -> m (Cell m a)
newEmptyCell = flip newCell top
