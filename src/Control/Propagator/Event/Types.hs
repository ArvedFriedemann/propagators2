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
    Write :: Identifier i a => Scope -> i -> a -> Write

deriving instance Show Write
instance Eq Write where
    a == b = compare a b == EQ
instance Ord Write where
    Write sA i a `compare` Write sB j b
        = compare sA sB
        <> compareTyped i j
        <> compareTyped a b


data Watch m where
    Watch :: (Identifier i a, Std j) => Scope -> i -> j -> (a -> m ()) -> Watch m

instance Show (Watch m) where
    showsPrec d (Watch s i j _)
        = showParen (d >= 10)
        $ showString "Watch "
        . shows s
        . showString " "
        . shows i
        . showString " "
        . shows j
instance Eq (Watch m) where
    a == b = compare a b == EQ
instance Ord (Watch m) where
    Watch sA iA jA _ `compare` Watch sB iB jB _
        = compare sA sB
        <> compareTyped iA iB
        <> compareTyped jA jB


data Fork m where
    Fork :: Std i => Scope -> i -> (LiftParent m -> m ()) -> Fork m

instance Show (Fork m) where
    showsPrec d (Fork s i _)
        = showParen (d >= 10)
        $ showString "Fork "
        . shows s
        . showString " "
        . shows i
instance Eq (Fork m) where
    a == b = compare a b == EQ
instance Ord (Fork m) where
    Fork s i _ `compare` Fork s' j _ = compare s s' <> compareTyped i j


data Event m
    = WriteEvt Write
    | WatchEvt (Watch m)
    | ForkEvt (Fork m)
  deriving (Eq, Ord, Generic)

instance MonadProp m => Show (Event m) where
    showsPrec d (WriteEvt e) = showsPrec d e
    showsPrec d (WatchEvt e) = showsPrec d e
    showsPrec d (ForkEvt e) = showsPrec d e
