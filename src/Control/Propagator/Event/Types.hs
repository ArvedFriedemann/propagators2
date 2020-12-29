module Control.Propagator.Event.Types where

import "base" GHC.Generics
import "base" Data.Foldable
import "base" Data.List.NonEmpty ( NonEmpty, (<|) )
import "base" Data.List.NonEmpty qualified as NonEmpty

import "deepseq" Control.DeepSeq

import "this" Control.Propagator.Class
import "this" Data.Typed
import "this" Data.Id

-------------------------------------------------------------------------------
-- Scope
-------------------------------------------------------------------------------

newtype Scope = Scope (NonEmpty Id)
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Semigroup, NFData)
instance Show Scope where
    show (Scope ids) = fold . NonEmpty.intersperse "/" . fmap show . NonEmpty.reverse $ ids

parentScope :: Scope -> Maybe Scope
parentScope (Scope p) = fmap Scope . snd . NonEmpty.uncons $ p

appendScope :: Id -> Scope -> Scope
appendScope i (Scope p) = Scope (i <| p)

-------------------------------------------------------------------------------
-- Event
-------------------------------------------------------------------------------

data Create m where
    Create :: Value a => Scope -> Cell m a -> a -> Create m

deriving instance PropagatorMonad m => Show (Create m)
instance PropagatorMonad m => Eq (Create m) where
    a == b = compare a b == EQ
instance PropagatorMonad m => Ord (Create m) where
    Create sA cA a `compare` Create sB cB b
        = compare sA sB
        <> compareTyped cA cB
        <> compareTyped a b


data Write m where
    Write :: Value a => Scope -> Cell m a -> a -> Write m

deriving instance PropagatorMonad m => Show (Write m)
instance PropagatorMonad m => Eq (Write m) where
    a == b = compare a b == EQ
instance PropagatorMonad m => Ord (Write m) where
    Write sA cA a `compare` Write sB cB b
        = compare sA sB
        <> compareTyped cA cB
        <> compareTyped a b


data Watch m where
    Watch :: Value a => Scope -> Cell m a -> Id -> (a -> m ()) -> Watch m

instance PropagatorMonad m => Show (Watch m) where
    showsPrec d (Watch s c i _)
        = showParen (d >= 10)
        $ showString "Watch "
        . shows s
        . showString " "
        . shows c
        . showString " "
        . shows i
instance PropagatorMonad m => Eq (Watch m) where
    a == b = compare a b == EQ
instance PropagatorMonad m => Ord (Watch m) where
    Watch sA cA a _ `compare` Watch sB cB b _
        = compare sA sB
        <> compareTyped cA cB
        <> compare a b


data Fork m where
    Fork :: Scope -> Id -> (LiftParent m -> m ()) -> Fork m

instance PropagatorMonad m => Show (Fork m) where
    showsPrec d (Fork s i _)
        = showParen (d >= 10)
        $ showString "Fork "
        . shows s
        . showString " "
        . shows i
instance PropagatorMonad m => Eq (Fork m) where
    a == b = compare a b == EQ
instance PropagatorMonad m => Ord (Fork m) where
    Fork sA cA _ `compare` Fork sB cB _
        = compare sA sB
        <> compare cA cB


data Cancel m where
    Cancel :: Subscription m -> Cancel m

deriving instance PropagatorMonad m => Show (Cancel m)
instance PropagatorMonad m => Eq (Cancel m) where
    a == b = compare a b == EQ
instance PropagatorMonad m => Ord (Cancel m) where
    Cancel sA `compare` Cancel sB
        = compareTyped sA sB


data Event m
    = CreateEvt (Create m)
    | WriteEvt (Write m)
    | WatchEvt (Watch m)
    | ForkEvt (Fork m)
    | CancelEvt (Cancel m)
  deriving (Eq, Ord, Generic)

instance PropagatorMonad m => Show (Event m) where
    showsPrec d (CreateEvt e) = showsPrec d e
    showsPrec d (WriteEvt e) = showsPrec d e
    showsPrec d (WatchEvt e) = showsPrec d e
    showsPrec d (ForkEvt e) = showsPrec d e
    showsPrec d (CancelEvt e) = showsPrec d e
