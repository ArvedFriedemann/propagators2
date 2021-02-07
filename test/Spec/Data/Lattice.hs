{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Data.Lattice ( tests ) where

import "base" Data.Monoid ( Dual(..) )

import "containers" Data.Set ( Set )

import "tasty" Test.Tasty

import "this" Data.Lattice
import "this" Spec.Util
import "this" Spec.Data.Lattice.Arbitrary ()
import "this" Spec.Data.Lattice.Laws qualified as Laws


tests :: TestTree
tests = testGroup "Data.Lattice"
    [ tgrp @(Dual (Ordered Ordering)) $ Laws.boundedLattice @(Dual (Ordered Ordering))
    , tgrp @(Set Int) $ Laws.lattice @(Set Int) ++ Laws.boundedjoin @(Set Int)
    , tgrp @(Ordered Ordering) $ Laws.boundedLattice @(Ordered Ordering)
    , tgrp @() $ Laws.boundedLattice @()
    , tgrp @(Bool, Bool) $ Laws.boundedLattice @(Bool, Bool)
    , tgrp @(Bool, Bool, Bool) $ Laws.boundedLattice @(Bool, Bool, Bool)
    , tgrp @Bool $ Laws.boundedLattice @Bool
    , tgrp @(WithBounds (Set Ordering)) $ Laws.boundedLattice @(WithBounds (Set Ordering))
    , tgrp @(Facts Ordering) $ Laws.boundedLattice @(Facts Ordering)
    ]
