{-# OPTIONS_GHC -fno-warn-orphans #-}
module Spec.Data.Lattice.Arbitrary where

import "base" Data.Functor.Compose

import "tasty-quickcheck" Test.Tasty.QuickCheck ( Arbitrary(..), Arbitrary1(..), frequency )

import "this" Data.Lattice


instance Arbitrary1 Ordered where
    liftArbitrary = fmap Ordered
instance Arbitrary a => Arbitrary (Ordered a) where
    arbitrary = liftArbitrary arbitrary

instance Arbitrary1 Monoidal where
    liftArbitrary = fmap Monoidal
instance Arbitrary a => Arbitrary (Monoidal a) where
    arbitrary = liftArbitrary arbitrary

instance Arbitrary1 WithBounds where
    liftArbitrary = fmap (WithBounds . Compose) . liftArbitrary . liftArbitrary
instance Arbitrary a => Arbitrary (WithBounds a) where
    arbitrary = liftArbitrary arbitrary

instance Arbitrary1 WithTop where
    liftArbitrary g = frequency [(1, pure SynthTop), (100, NotTop <$> g)]
instance Arbitrary a => Arbitrary (WithTop a) where
    arbitrary = liftArbitrary arbitrary

instance Arbitrary1 WithBot where
    liftArbitrary g = frequency [(1, pure SynthBot), (100, NotBot <$> g)]
instance Arbitrary a => Arbitrary (WithBot a) where
    arbitrary = liftArbitrary arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (Facts a) where
    arbitrary = frequency [(1, pure Bot), (100, Facts <$> arbitrary)]
