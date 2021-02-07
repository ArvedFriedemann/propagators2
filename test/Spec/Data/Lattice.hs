{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Spec.Data.Lattice ( tests ) where

import "tasty" Test.Tasty
import "tasty-quickcheck" Test.Tasty.QuickCheck ( Arbitrary(..) )

import "this" Data.Lattice
import "this" Spec.Data.Lattice.Laws qualified as Laws


instance Arbitrary a => Arbitrary (Ordered a) where
    arbitrary = Ordered <$> arbitrary
instance Arbitrary a => Arbitrary (Monoidal a) where
    arbitrary = Monoidal <$> arbitrary


tests :: TestTree
tests = testGroup "Data.Lattice"
    [ Laws.tgrp @() $ Laws.boundedLattice @()
    , Laws.tgrp @Bool $ Laws.boundedLattice @Bool
    , Laws.tgrp @(Bool, Bool) $ Laws.boundedLattice @(Bool, Bool)
    , Laws.tgrp @(Ordered Ordering) $ Laws.boundedLattice @(Ordered Ordering)
    , Laws.tgrp @(Bool, Bool, Bool) $ Laws.boundedLattice @(Bool, Bool, Bool)
    ]

