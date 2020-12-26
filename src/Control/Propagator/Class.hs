{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StaticPointers    #-}
module Control.Propagator.Class
    ( PropagatorMonad(..)
    , PropagatorEqMonad(..)
    , LiftParent
    , Forkable(..)
    , Value
    , BoundedValue
    , newEmptyCell
    , staticWrite
    {-, linkM
    , linkM2
    , link
    , link2-}
    , eqAll
    ) where

import "base" Prelude hiding ( (.), id )
import "base" Control.Category
import "base" Data.Typeable
import "base" Data.Type.Equality

import "this" Data.Iso
import "this" Data.Lattice
import "this" Data.Ref


class (Meet a, Refable a) => Value a
instance (Meet a, Refable a) => Value a

class (BoundedMeet a, Value a) => BoundedValue a
instance (BoundedMeet a, Value a) => BoundedValue a


class ( forall a. Show (Cell m a)
      , forall a. Ord (Cell m a)
      , TestEquality (Cell m)
      , Eq (Subscription m)
      , Show (Subscription m)
      , Monoid (Subscription m)
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
    watch :: Value a => Cell m a -> Ref (a -> m ()) -> m (Subscription m)
    cancel :: Subscription m -> m ()


class PropagatorMonad m => PropagatorEqMonad m where
    iso :: (Value a, Value b) => Cell m a -> Cell m b -> Ref (a <-> b) -> m ()
    eq :: (Value a) => Cell m a -> Cell m a -> m ()
    eq a b = iso a b (static id)

eqAll :: (PropagatorEqMonad m, Value a) => [Cell m a] -> m ()
eqAll [] = pure ()
eqAll (a : as) = mapM_ (eq a) as

-------------------------------------------------------------------------------
-- combinators
-------------------------------------------------------------------------------
{-
linkM :: ( PropagatorMonad m, Value a, Value b)
      => Cell m a -> Cell m b -> (a -> m b) -> m (Subscription m)
linkM ca cb f = watch ca $ \ a -> f a >>= write cb

linkM2 :: ( PropagatorMonad m, Value a, Value b, Value c)
       => Cell m a -> Cell m b -> Cell m c -> (a -> b -> m c) -> m (Subscription m)
linkM2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a =<< readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b =<< readCell ca
    pure (unsubCa <> unsubCb)

link :: ( PropagatorMonad m, Value a, Value b)
     =>  Cell m a -> Cell m b -> (a -> b) -> m (Subscription m)
link ca cb f = linkM ca cb $ pure . f

link2 :: ( PropagatorMonad m, Value a, Value b, Value c)
      => Cell m a -> Cell m b -> Cell m c -> (a -> b -> c) -> m (Subscription m)
link2 ca cb cc f = do
    unsubCa <- linkM ca cc $ \ a -> f a <$> readCell cb
    unsubCb <- linkM cb cc $ \ b -> flip f b <$> readCell ca
    pure (unsubCa <> unsubCb)
-}
newEmptyCell :: forall a m. (PropagatorMonad m, BoundedValue a) => String -> m (Cell m a)
newEmptyCell = flip newCell top


staticWrite :: forall a m. (Typeable m, Value a, PropagatorMonad m) => Ref (Cell m a -> a -> m ())
staticWrite = defere @(Value a, PropagatorMonad m) (static \Dict -> write)

type LiftParent m = forall a. m a -> m a

class Applicative m => Forkable m where
    fork :: (LiftParent m -> m ()) -> m ()
