{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Data.Lattice.Laws where

import "tasty" Test.Tasty
import "tasty-quickcheck" Test.Tasty.QuickCheck

import "this" Data.Lattice


meet ::  forall l. (Arbitrary l, Eq l, Show l, Meet l) => [TestTree]
meet = 
    [ testProperty "Associativity: x ⋀ (y ⋀ z) = (x ⋀ y) ⋀ z" $
        \ (x :: l) y z -> x ⋀ (y ⋀ z) === (x ⋀ y) ⋀ z
    , testProperty "Commutativity: x ⋀ y = y ⋀ x" $
        \ (x :: l) y -> x ⋀ y === y ⋀ x
    , testProperty "Idempotency: x ⋀ x = x" $
        \ (x :: l) -> x ⋀ x === x
    ]

boundedmeet :: forall l. (Arbitrary l, Eq l, Show l, BoundedMeet l) => [TestTree]
boundedmeet = meet @l ++
    [ testProperty "Identity: x ⋀ top = x" $
        \ (x :: l) -> x ⋀ top === x
    ]

join :: forall l. (Arbitrary l, Eq l, Show l, Join l) => [TestTree]
join =
    [ testProperty "Associativity: x ⋁ (y ⋁ z) = (x ⋁ y) ⋁ z" $
        \ (x :: l) y z -> x ⋁ (y ⋁ z) === (x ⋁ y) ⋁ z
    , testProperty "Commutativity: x ⋁ y = y ⋁ x" $
        \ (x :: l) y -> x ⋁ y === y ⋁ x
    , testProperty "Idempotency: x ⋁ x = x" $
        \ (x :: l) -> x ⋁ x === x
    ]

boundedjoin :: forall l. (Arbitrary l, Eq l, Show l, BoundedJoin l) => [TestTree]
boundedjoin = join @l ++
    [ testProperty "Identity: x ⋁ bot = x" $
        \ (x :: l) -> x ⋁ bot === x
    ]

lattice :: forall l. (Arbitrary l, Eq l, Show l, Lattice l) => [TestTree]
lattice = meet @l ++ join @l ++
    [ testProperty "AbsorptionLeft: a ⋁ (a ⋀ b) = a" $
        \ (a :: l) b -> a ⋁ (a ⋀ b) === a
    , testProperty "AbsorptionRight: a ⋀ (a ⋁ b) = a" $
        \ (a :: l) b -> a ⋀ (a ⋁ b) === a
    ]

boundedLattice :: forall l. (Arbitrary l, Eq l, Show l, BoundedLattice l) => [TestTree]
boundedLattice = boundedmeet @l ++ boundedjoin @l ++ (reverse . take 2 . reverse $ lattice @l) ++
    [ testProperty "MeetIdentity: bot ⋀ b = bot" $
        \ (b :: l) -> bot ⋀ b === bot
    , testProperty "JoinIdentity: top ⋁ b = top" $
        \ (b :: l) -> top ⋁ b === top
    ]
