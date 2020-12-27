{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Class
    ( PropagatorMonad(..)
    , Subscriptions(..)
    , Scoped(..)
    , LiftParent
    , Forkable(..)
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
import "base" Data.Functor
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
      , Std (Id m)
      , Typeable m
      , Monad m
      ) => PropagatorMonad m where

    data Cell m :: * -> *

    data Subscription m

    data Id m

    newCell :: (Std b, Value a) => b -> a -> m (Cell m a)

    readCell :: Value a => Cell m a -> m a

    write :: Value a => Cell m a -> a -> m ()

    watch :: (Std b, Value a) => Cell m a -> b -> (a -> m ()) -> m (Subscriptions m)

    cancel :: Subscriptions m -> m ()


type LiftParent m = forall a. m a -> m a

class Applicative m => Forkable m where
    fork :: Std b => b -> (LiftParent m -> m ()) -> m ()

class PropagatorMonad m => Scoped m where

    scope :: String -> m a -> m a

    scope' :: m a -> m a
    scope' = scope "anon"
    
    currentScope :: m (Id m)

-------------------------------------------------------------------------------
-- combinators
-------------------------------------------------------------------------------

iso :: (Scoped m, Value a, Value b) => Cell m a -> Cell m b -> (a <-> b) -> m (Subscriptions m)
iso ca cb i = scope "iso" $ do
    sa <- watch ca ("to" :: String) $ write cb . to i
    sb <- watch cb ("from" :: String) $ write ca . from i
    pure (sa <> sb)

eq :: (Scoped m, Value a) => Cell m a -> Cell m a -> m (Subscriptions m)
eq a b = iso a b id

eqAll :: (Scoped m, Value a) => [Cell m a] -> m (Subscriptions m)
eqAll [] = pure mempty
eqAll (a : as) = fold <$> mapM (eq a) as

linkM :: (Scoped m, Value a, Value b, Std l)
      => Cell m a -> Cell m b -> l -> (a -> m b) -> m (Subscriptions m)
linkM ca cb l f = watch ca ("linkM" :: String, cb, l) $ \ a -> f a >>= write cb

linkM2 :: (Scoped m, Value a, Value b, Value c, Std l)
       => Cell m a -> Cell m b -> Cell m c -> l -> (a -> b -> m c) -> m (Subscriptions m)
linkM2 ca cb cc l f = do
    unsubCa <- linkM ca cc l $ \ a -> f a =<< readCell cb
    unsubCb <- linkM cb cc l $ \ b -> flip f b =<< readCell ca
    pure (unsubCa <> unsubCb)

link :: (Scoped m, Value a, Value b, Std l)
     =>  Cell m a -> Cell m b -> l -> (a -> b) -> m (Subscriptions m)
link ca cb l f = linkM ca cb l $ pure . f

link2 :: (Scoped m, Value a, Value b, Value c, Std l)
      => Cell m a -> Cell m b -> Cell m c -> l -> (a -> b -> c) -> m (Subscriptions m)
link2 ca cb cc l f = linkM2 ca cb cc l $ \ a b -> pure $ f a b

newEmptyCell :: forall a m. (Scoped m, BoundedValue a) => String -> m (Cell m a)
newEmptyCell = flip newCell top
