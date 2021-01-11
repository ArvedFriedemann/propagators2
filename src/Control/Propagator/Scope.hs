module Control.Propagator.Scope where

import "base" GHC.Exts ( IsList(..) )

import "this" Data.Typed
import "this" Data.Some
import "this" Control.Propagator.Class


data Scope where
    Root :: Scope
    Scope :: Std i => i -> Scope -> Scope

instance Eq Scope where
    Root == Root = True
    Scope i a == Scope j b = i =~= j && a == b
    _ == _ = False
instance Ord Scope where
    Root `compare` Root = EQ
    Root `compare` _ = GT
    _ `compare` Root = LT
    Scope i a `compare` Scope j b = compareTyped i j <> compare a b
instance Show Scope where
    showsPrec _ Root = showString "/"
    showsPrec _ (Scope i s) = shows s . showsPrec 11 i . showString "/"

instance Semigroup Scope where
    Root <> a = a
    Scope i a <> b = Scope i (a <> b)
instance Monoid Scope where
    mempty = Root

instance IsList Scope where
    type Item Scope = Some Std
    fromList (Some i : as) = Scope i $ fromList as
    fromList [] = Root
    toList (Scope i a) = Some i : toList a
    toList Root = []

pushScope :: Std i => i -> Scope -> Scope
pushScope = Scope

popScope :: Scope -> Maybe (Some Std, Scope)
popScope (Scope i s) = pure (Some i, s)
popScope _ = Nothing

{-| longest common prefix of two Scopes.

  Let @(l, r, p) = lcp a b@ then @a = p <> l@ and @b = p <> r@.
-}
lcp :: Scope -> Scope -> (Scope, Scope, Scope)
lcp (Scope i a) (Scope j b) | i =~= j
    = let (l, r, p) = lcp a b
       in (l, r, Scope i p)
lcp a b = (a, b, Root)
