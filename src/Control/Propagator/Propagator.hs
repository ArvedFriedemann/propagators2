module Control.Propagator.Propagator where

import "base" Data.Ord
import "base" Data.Foldable
import "base" Data.Semigroup as S
import "base" Data.Monoid as M
import "base" Data.List.NonEmpty
import "base" Data.Functor.Identity
import "base" Data.Typeable
import "base" Control.Applicative

import "containers" Data.Set ( Set )

import "this" Control.Propagator.Class
import "this" Data.Lattice


class (Std p, Std a) => Propagator m a p where
    propagate :: p -> a -> m ()

instance (Applicative m, Propagator m a i, Propagator m a j) => Propagator m a (Either i j) where
    propagate = either propagate propagate
instance (Applicative m, Propagator m a i, Propagator m a j) => Propagator m a (i, j) where
    propagate (i, j) a = propagate i a *> propagate j a
instance (Applicative m, Propagator m a i, Propagator m a j, Propagator m a k) => Propagator m a (i, j, k) where
    propagate (i, j, k) a = propagate i a *> propagate j a *> propagate k a

data UniversalPropagator m = UniversalPropagator (m ())
instance Eq (UniversalPropagator m) where
  _ == _ = False
instance Ord (UniversalPropagator m) where
  compare _ _ = EQ
instance Show (UniversalPropagator m) where
  show _ = "UniversalPropagator"
instance (Typeable m, Std a) => Propagator m a (UniversalPropagator m) where
  propagate (UniversalPropagator m) _ = m

data UniversalWatchPropagator a m = UniversalWatchPropagator (a -> m ())
instance Eq (UniversalWatchPropagator a m) where
  _ == _ = False
instance Ord (UniversalWatchPropagator a m) where
  compare _ _ = EQ
instance Show (UniversalWatchPropagator a m) where
  show _ = "UniversalWatchPropagator"
instance (Typeable m, Std a) => Propagator m a (UniversalWatchPropagator a m) where
  propagate (UniversalWatchPropagator m) x = m x

instance ( Applicative m, Typeable f, Std i, Std (f i)
         , Foldable f, Propagator m a i
         ) => Propagator m a (Applied f i) where
    propagate (Applied is) a
        = traverse_ (flip propagate a) is

instance (Applicative m, Propagator m a i) => Propagator m a [i] where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Maybe i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (NonEmpty i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Down i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Product i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Sum i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Dual i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (S.Last i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (S.First i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Identity i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (ZipList i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Option i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (M.Last i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (M.First i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Max i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Min i) where
    propagate = propagate . Applied
instance (Applicative m, Propagator m a i) => Propagator m a (Set i) where
    propagate = propagate . Applied
