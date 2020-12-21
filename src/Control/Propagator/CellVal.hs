module Control.Propagator.CellVal where

import "base" Data.Function ( on )
import "this" Data.Iso
import "this" Data.Lattice
import "this" Data.Var
import "this" Control.Propagator.Class


data CellVal m a where
    Val :: !a -> !(Listeners m a) -> ![Mapping m a] -> CellVal m a
    Ref :: (Meet b, Ord b)
        => !(Cell m b) -> !(b <-> a) -> CellVal m a


type Listeners m a = Pool (a -> m ())
type Listener m a = Id (Pool (a -> m ())) (a -> m ())

data Mapping m a where
    Mapping :: !(Cell m b) -> !(b <-> a) -> !(Listeners m b) -> Mapping m a

data Sub m where
    Sub :: forall a m
        . ( Show (Cell m a), Show (Listener m a))
        => !(Cell m a) -> !(Listener m a) -> Sub m

instance Show (Sub m) where
    showsPrec d (Sub c l)
        = showParen (d > 10)
        $ shows c
        . showString " -> "
        . shows l

instance Eq (Sub m) where
    (==) = (==) `on` show
instance Ord (Sub m) where
    compare = compare `on` show
