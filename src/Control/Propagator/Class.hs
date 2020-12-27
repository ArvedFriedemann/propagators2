{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Class
    ( PropagatorMonad(..)
    , Subscriptions(..)
    , LiftParent
    , Forkable(..)
    , Value
    , BoundedValue
    , newEmptyCell
    , iso
    , eq
    , linkM
    , linkM2
    , link
    , link2
    , eqAll
    ) where

import "base" Prelude hiding ( (.), id )
import "base" Data.Foldable
import "base" Data.Typeable
import "base" Data.Type.Equality
import "base" Control.Category

import "this" Data.Iso
import "this" Data.Lattice


class (Meet a, Ord a, Typeable a, Show a) => Value a
instance (Meet a, Ord a, Typeable a, Show a) => Value a

class (BoundedMeet a, Value a) => BoundedValue a
instance (BoundedMeet a, Value a) => BoundedValue a

newtype Subscriptions m = Subscriptions
    { getSubscriptions :: [Subscription m]
    }
  deriving newtype (Semigroup, Monoid)

deriving stock instance Show (Subscription m) => Show (Subscriptions m)
deriving stock instance Eq (Subscription m) => Eq (Subscriptions m)

class ( forall a. Show (Cell m a)
      , forall a. Ord (Cell m a)
      , TestEquality (Cell m)
      , Eq (Subscription m)
      , Show (Subscription m)
      , Typeable m
      , Monad m
      ) => PropagatorMonad m where

    data Cell m :: * -> *

    data Subscription m

    newCell :: Value a => String -> a -> m (Cell m a)

    newCell' :: Value a => a -> m (Cell m a)
    newCell' = newCell ""

    readCell :: Value a => Cell m a -> m a

    write :: Value a => Cell m a -> a -> m ()

    watch :: Value a => Cell m a -> (a -> m ()) -> m (Subscriptions m)

    cancel :: Subscriptions m -> m ()

type LiftParent m = forall a. m a -> m a

class Applicative m => Forkable m where
    fork :: (LiftParent m -> m ()) -> m ()
    
-------------------------------------------------------------------------------
-- combinators
-------------------------------------------------------------------------------

iso :: (PropagatorMonad m, Value a, Value b) => Cell m a -> Cell m b -> (a <-> b) -> m (Subscriptions m)
iso ca cb i = do
    sa <- watch ca $ write cb . to i
    sb <- watch cb $ write ca . from i
    pure (sa <> sb)

eq :: (PropagatorMonad m, Value a) => Cell m a -> Cell m a -> m (Subscriptions m)
eq a b = iso a b id

eqAll :: (PropagatorMonad m, Value a) => [Cell m a] -> m (Subscriptions m)
eqAll [] = pure mempty
eqAll (a : as) = fold <$> mapM (eq a) as

linkM :: ( PropagatorMonad m, Value a, Value b)
      => Cell m a -> Cell m b -> (a -> m b) -> m (Subscriptions m)
linkM ca cb f = watch ca $ \ a -> f a >>= write cb

linkM2 :: ( PropagatorMonad m, Value a, Value b, Value c)
       => Cell m a -> Cell m b -> Cell m c -> (a -> b -> m c) -> m (Subscriptions m)
linkM2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a =<< readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b =<< readCell ca
    pure (unsubCa <> unsubCb)

link :: ( PropagatorMonad m, Value a, Value b)
     =>  Cell m a -> Cell m b -> (a -> b) -> m (Subscriptions m)
link ca cb f = linkM ca cb $ pure . f

link2 :: ( PropagatorMonad m, Value a, Value b, Value c)
      => Cell m a -> Cell m b -> Cell m c -> (a -> b -> c) -> m (Subscriptions m)
link2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a <$> readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b <$> readCell ca
    pure (unsubCa <> unsubCb)

newEmptyCell :: forall a m. (PropagatorMonad m, BoundedValue a) => String -> m (Cell m a)
newEmptyCell = flip newCell top
