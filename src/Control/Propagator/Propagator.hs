module Control.Propagator.Propagator where

import "base" Data.Ord
import "base" Data.Foldable
import "base" Data.Semigroup as S
import "base" Data.Monoid as M
import "base" Data.List.NonEmpty
import "base" Data.Functor.Identity
import "base" Data.Typeable
import "base" Control.Applicative

import "hashable" Data.Hashable

import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Hashable.Orphans ()


class (Std p, Std a) => Propagator m a p where
    propagate :: p -> a -> m ()

instance (Applicative m, Propagator m a i, Propagator m a j) => Propagator m a (Either i j) where
    propagate = either propagate propagate
instance (Applicative m, Propagator m a i, Propagator m a j) => Propagator m a (i, j) where
    propagate (i, j) a = propagate i a *> propagate j a
instance (Applicative m, Propagator m a i, Propagator m a j, Propagator m a k) => Propagator m a (i, j, k) where
    propagate (i, j, k) a = propagate i a *> propagate j a *> propagate k a


data UniversalNamedPropagator n m = UniversalNamedPropagator n (m ())
instance Hashable n => Hashable (UniversalNamedPropagator n m) where
    hashWithSalt s (UniversalNamedPropagator n _) = hashWithSalt s n
instance (Eq n) => Eq (UniversalNamedPropagator n m) where
    (UniversalNamedPropagator n1 _) == (UniversalNamedPropagator n2 _) = n1 == n2
instance (Ord n) => Ord (UniversalNamedPropagator n m) where
    compare (UniversalNamedPropagator n1 _) (UniversalNamedPropagator n2 _) = compare n1 n2
instance (Show n) => Show (UniversalNamedPropagator n m) where
    show (UniversalNamedPropagator n _) = "UniversalNamedPropagator "++show n
instance (Typeable m, Std a, Std n) => Propagator m a (UniversalNamedPropagator n m) where
    propagate (UniversalNamedPropagator _ m) _ = m


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
{-
instance (Applicative m, Propagator m a i) => Propagator m a (Set i) where
    propagate = propagate . Applied
-}