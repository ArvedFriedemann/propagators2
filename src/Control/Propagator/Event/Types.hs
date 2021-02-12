module Control.Propagator.Event.Types where

import "base" GHC.Generics

import "hashable" Data.Hashable

import "this" Control.Propagator.Class
import "this" Control.Propagator.Base
import "this" Control.Propagator.Scope
import "this" Control.Propagator.Propagator
import "this" Data.Typed


-------------------------------------------------------------------------------
-- Event
-------------------------------------------------------------------------------

data Write where
    Write :: Identifier i a => i -> a -> Scope -> Write

deriving instance Show Write
instance Hashable Write where
    hashWithSalt n (Write i a s) = hashWithSalt n (i, a, s)
instance Eq Write where
    Write i a s == Write j b t = i =~= j && a =~= b && s == t
instance Ord Write where
    Write i a sA `compare` Write j b sB
        = compareTyped i j
        <> compareTyped a b
        <> compare sA sB

data WatchFixpoint m where
    WatchFixpoint :: Std i => i -> m () -> Scope -> WatchFixpoint m

instance Show (WatchFixpoint m) where
    showsPrec d (WatchFixpoint i _ s)
        = showParen (d >= 10)
        $ showString "WatchFixpoint "
        . shows i
        . showString " "
        . shows s
instance Eq (WatchFixpoint m) where
    WatchFixpoint a _ s == WatchFixpoint b _ t = a =~= b && s == t
instance Hashable (WatchFixpoint m) where
    hashWithSalt n (WatchFixpoint i _ s) = hashWithSalt n (i, s)
instance Ord (WatchFixpoint m) where
    WatchFixpoint a _ s `compare` WatchFixpoint b _ t = compareTyped a b <> compare s t


data Watch m where
    Watch :: (Identifier i a, Propagator m a p) => i -> p -> Scope -> Watch m

instance Show (Watch m) where
    showsPrec d (Watch i p s)
        = showParen (d >= 10)
        $ showString "Watch "
        . shows i
        . showString " "
        . shows p
        . showString " "
        . shows s
instance Eq (Watch m) where
    a == b = compare a b == EQ
instance Hashable (Watch m) where
    hashWithSalt n (Watch i p s) = hashWithSalt n (i, p, s)
instance Ord (Watch m) where
    Watch iA pA sA `compare` Watch iB pB sB
        = compareTyped iA iB
        <> compareTyped pA pB
        <> compare sA sB


data Event m
    = WriteEvt Write
    | WatchEvt (Watch m)
    | WatchFixpointEvt (WatchFixpoint m)
  deriving (Eq, Ord, Generic)
instance Hashable (Event m)
instance MonadProp m => Show (Event m) where
    showsPrec d (WriteEvt e) = showsPrec d e
    showsPrec d (WatchEvt e) = showsPrec d e
    showsPrec d (WatchFixpointEvt e) = showsPrec d e
