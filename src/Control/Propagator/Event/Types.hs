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
    Watch :: (Identifier i a, Std j) => i -> j -> (a -> m ()) -> Scope -> Watch m

instance Show (Watch m) where
    showsPrec d (Watch i j _ s)
        = showParen (d >= 10)
        $ showString "Watch "
        . shows i
        . showString " "
        . shows j
        . showString " "
        . shows s
instance Eq (Watch m) where
    a == b = compare a b == EQ
instance Ord (Watch m) where
    Watch iA jA _ sA `compare` Watch iB jB _ sB
        = compareTyped iA iB
        <> compareTyped jA jB
        <> compare sA sB


data Fork m where
    Fork :: Std i => i -> (LiftParent m -> m x) -> Scope -> Fork m

instance Show (Fork m) where
    showsPrec d (Fork i _ s)
        = showParen (d >= 10)
        $ showString "Fork "
        . shows i
        . showString " "
        . shows s
instance Eq (Fork m) where
    a == b = compare a b == EQ
instance Ord (Fork m) where
    Fork i _ s `compare` Fork j _ s' = compareTyped i j <> compare s s'


data Event m
    = WriteEvt Write
    | WatchEvt (Watch m)
    | ForkEvt (Fork m)
  deriving (Eq, Ord, Generic)

instance MonadProp m => Show (Event m) where
    showsPrec d (WriteEvt e) = showsPrec d e
    showsPrec d (WatchEvt e) = showsPrec d e
    showsPrec d (ForkEvt e) = showsPrec d e
