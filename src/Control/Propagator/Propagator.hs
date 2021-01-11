module Control.Propagator.Propagator where

import "base" Data.Ord
import "base" Data.Semigroup as S
import "base" Data.Monoid as M
import "base" Data.List.NonEmpty
import "base" Data.Functor.Identity
import "base" Data.Typeable
import "base" Control.Applicative

import "containers" Data.Set ( Set )

import "this" Control.Propagator.Class
import "this" Data.Lattice


class (Std p, Std a) => Propagator m p a where
    propagate :: p -> a -> m ()

instance (Applicative m, Propagator m i a, Propagator m j a) => Propagator m (Either i j) a where
    propagate = either propagate propagate
instance (Applicative m, Propagator m i a, Propagator m j a) => Propagator m (i, j) a where
    propagate (i, j) a = propagate i a *> propagate j a
instance (Applicative m, Propagator m i a, Propagator m j a, Propagator m k a) => Propagator m (i, j, k) a where
    propagate (i, j, k) a = propagate i a *> propagate j a *> propagate k a


instance ( Applicative m, Typeable f, Std i, Std (f i)
         , Foldable f, Propagator m i a
         ) => Propagator m (Applied f i) a where
    propagate (Applied is) a
        = foldr (\i m -> propagate i a *> m) (pure ()) is

instance (Applicative m, Propagator m i a) => Propagator m [i] a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Maybe i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (NonEmpty i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Down i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Product i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Sum i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Dual i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (S.Last i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (S.First i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Identity i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (ZipList i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Option i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (M.Last i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (M.First i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Max i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Min i) a where
    propagate = propagate . Applied
instance (Applicative m, Propagator m i a) => Propagator m (Set i) a where
    propagate = propagate . Applied
