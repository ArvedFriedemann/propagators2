module Control.Propagator.Event.Types where

import "base" GHC.Generics
import "base" Data.Typeable

import "this" Control.Propagator.Class
import "this" Data.Typed

-------------------------------------------------------------------------------
-- Scope
-------------------------------------------------------------------------------

data Scope where
    Root :: Scope
    Scope :: Std i => i -> Scope -> Scope

instance Eq Scope where
    Root == Root = True
    Scope i a == Scope j b = cast i == Just j && a == b
    _ == _ = False
instance Ord Scope where
    Root `compare` Root = EQ
    Root `compare` _ = GT
    _ `compare` Root = LT
    Scope i a `compare` Scope j b = compareTyped i j <> compare a b
instance Show Scope where
    showsPrec _ Root = showString "/"
    showsPrec _ (Scope i s) = showString "/" . showsPrec 11 i . shows s


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
    Write :: Identifier i a => i -> a -> Scope -> Write

deriving instance Show Write
instance Eq Write where
    a == b = compare a b == EQ
instance Ord Write where
    Write i a sA `compare` Write j b sB
        = compareTyped i j
        <> compareTyped a b
        <> compare sA sB


data Watch m where
    Watch :: (Identifier i a, Propagator m p a) => i -> p -> Scope -> Watch m

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


data Fork m where
    Fork :: Forked m i => i -> Scope -> Fork m

instance Show (Fork m) where
    showsPrec d (Fork i s)
        = showParen (d >= 10)
        $ showString "Fork "
        . shows i
        . showString " "
        . shows s
instance Eq (Fork m) where
    a == b = compare a b == EQ
instance Ord (Fork m) where
    Fork i s `compare` Fork j s' = compareTyped i j <> compare s s'


data Event m
    = WriteEvt Write
    | WatchEvt (Watch m)
    | ForkEvt (Fork m)
  deriving (Eq, Ord, Generic)

instance MonadProp m => Show (Event m) where
    showsPrec d (WriteEvt e) = showsPrec d e
    showsPrec d (WatchEvt e) = showsPrec d e
    showsPrec d (ForkEvt e) = showsPrec d e
