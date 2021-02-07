module Spec.Control.Propagator.Scope ( tests ) where

import "base" GHC.Exts ( IsList(..) )

import "tasty" Test.Tasty
import "tasty-quickcheck" Test.Tasty.QuickCheck

import "this" Control.Propagator.Scope
import "this" Data.Some
import "this" Spec.Control.Propagator.Scope.Arbitrary ()

tests :: TestTree
tests = testGroup "Control.Propagator.Scope"
    [ testProperty "popScope (s :/ i) = Just (Some i, s)" $
        \ (s :: Scope) (i :: Int) -> popScope (s :/ i) === Just (Some i, s)
    , testProperty "pushScope i s = s :/ i" $
        \ (s :: Scope) (i :: Int) -> pushScope i s === (s :/ i)
    , testGroup "lcp"
        [ testProperty "Left Identity: let (l, r, p) = lcp a b in a = p <> l" $
            \ (a :: Scope) b -> let (l, _, p) = lcp a b in a === p <> l
        , testProperty "Right Identity: let (l, r, p) = lcp a b in b = p <> r" $
            \ (a :: Scope) b -> let (_, r, p) = lcp a b in b === p <> r
        ]
    , testGroup "IsList"
        [ testProperty "Identity: fromList . toList = id" $
            \ (s :: Scope) -> (fromList . toList $ s) === s
        , testProperty "Identity: toList . fromList = id" $
            \ s -> (toList . fromList @Scope $ s) === s
        ]
    , testGroup "Monoid"
        [ testProperty "Root = mzero" $ Root === mempty
        , testProperty "Right Identity: x <> mempty = x" $
            \ (x :: Scope) -> x <> mempty === x
        , testProperty "Left Identity: mempty <> x = x" $
            \ (x :: Scope) -> mempty <> x === x
        , testProperty "Associativity: x <> (y <> z) = (x <> y) <> z" $
            \ (x :: Scope) y z -> x <> (y <> z) === (x <> y) <> z
        , testProperty "Concatenation: mconcat = foldr (<>) mempty" $
            \ (s :: [Scope]) -> mconcat s === foldr (<>) mempty s
        ]
    ]
