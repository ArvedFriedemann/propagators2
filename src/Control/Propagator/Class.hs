{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Class
    ( PropagatorMonad(..)
    , watch
    , Subscriptions(..)
    , Scoped(..)
    , scope
    , LiftParent
    , Forkable(..)
    , fork
    , Std
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

class (Ord a, Typeable a, Show a) => Std a
instance (Ord a, Typeable a, Show a) => Std a

class (Meet a, Std a) => Value a
instance (Meet a, Std a) => Value a

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

    readCell :: Value a => Cell m a -> m a

    write :: Value a => Cell m a -> a -> m ()

    namedWatch :: Value a => Cell m a -> String -> (a -> m ()) -> m (Subscriptions m)

    cancel :: Subscriptions m -> m ()
    
watch :: (PropagatorMonad m, Value a) => Cell m a -> (a -> m ()) -> m (Subscriptions m)
watch c = namedWatch c ""

type LiftParent m = forall a. m a -> m a

class Applicative m => Forkable m where
    namedFork :: String -> (LiftParent m -> m ()) -> m ()

fork :: Forkable m => (LiftParent m -> m ()) -> m ()
fork = namedFork ""

class (Std (Scope m), PropagatorMonad m) => Scoped m where

    data Scope m

    namedScope :: String -> m a -> m a
    
    currentScope :: m (Scope m)

scope :: Scoped m => m a -> m a
scope = namedScope ("anon" :: String)

-------------------------------------------------------------------------------
-- combinators
-------------------------------------------------------------------------------

iso :: (PropagatorMonad m, Value a, Value b) => Cell m a -> Cell m b -> (a <-> b) -> m (Subscriptions m)
iso ca cb i = do
    sa <- namedWatch ca ("to" :: String) $ write cb . to i
    sb <- namedWatch cb ("from" :: String) $ write ca . from i
    pure (sa <> sb)

eq :: (PropagatorMonad m, Value a) => Cell m a -> Cell m a -> m (Subscriptions m)
eq a b = iso a b id

eqAll :: (PropagatorMonad m, Value a) => [Cell m a] -> m (Subscriptions m)
eqAll [] = pure mempty
eqAll (a : as) = fold <$> mapM (eq a) as

linkM :: (PropagatorMonad m, Value a, Value b)
      => Cell m a -> Cell m b -> (a -> m b) -> m (Subscriptions m)
linkM ca cb f = namedWatch ca ("linkM " ++ show cb) $ \ a -> f a >>= write cb

linkM2 :: (PropagatorMonad m, Value a, Value b, Value c)
       => Cell m a -> Cell m b -> Cell m c -> (a -> b -> m c) -> m (Subscriptions m)
linkM2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a =<< readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b =<< readCell ca
    pure (unsubCa <> unsubCb)

link :: (PropagatorMonad m, Value a, Value b)
     =>  Cell m a -> Cell m b -> (a -> b) -> m (Subscriptions m)
link ca cb f = linkM ca cb $ pure . f

link2 :: (PropagatorMonad m, Value a, Value b, Value c)
      => Cell m a -> Cell m b -> Cell m c -> (a -> b -> c) -> m (Subscriptions m)
link2 ca cb cc f = linkM2 ca cb cc $ \ a b -> pure $ f a b

newEmptyCell :: forall a m. (PropagatorMonad m, BoundedValue a) => String -> m (Cell m a)
newEmptyCell = flip newCell top
