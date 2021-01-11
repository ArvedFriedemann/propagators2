module Control.Propagator.Event.Types where

import "base" GHC.Generics

import "this" Control.Propagator.Base
import "this" Control.Propagator.Class
import "this" Control.Propagator.Scope
import "this" Control.Propagator.Propagator
import "this" Data.Typed


data SomePropagator m a where
    SomePropagator :: Propagator m p a => p -> SomePropagator m a
instance Eq (SomePropagator m a) where
    a == b = compare a b == EQ
instance Ord (SomePropagator m a) where
    SomePropagator a `compare` SomePropagator b = compareTyped a b
instance Show (SomePropagator m a) where
    showsPrec d (SomePropagator p)
        = showParen (d >= 10)
        $ showString "SomePropagator "
        . showsPrec 11 p
    showList = showList . fmap showElem
      where showElem (SomePropagator p) = show p

-------------------------------------------------------------------------------
-- Event
-------------------------------------------------------------------------------

data Write where
    Write :: (Value a, Identifier i a) => i -> a -> Scope -> Write

deriving instance Show Write
instance Eq Write where
    a == b = compare a b == EQ
instance Ord Write where
    Write i a sA `compare` Write j b sB
        = compareTyped i j
        <> compareTyped a b
        <> compare sA sB


data Watch m where
    Watch :: (Value a, Identifier i a, Propagator m p a) => i -> p -> Scope -> Watch m

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
instance Ord (Watch m) where
    Watch iA pA sA `compare` Watch iB pB sB
        = compareTyped iA iB
        <> compareTyped pA pB
        <> compare sA sB


data Event m
    = WriteEvt Write
    | WatchEvt (Watch m)
  deriving (Eq, Ord, Generic)

instance MonadProp m => Show (Event m) where
    showsPrec d (WriteEvt e) = showsPrec d e
    showsPrec d (WatchEvt e) = showsPrec d e
