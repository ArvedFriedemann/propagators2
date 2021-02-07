{-# OPTIONS_GHC -fno-warn-orphans #-}
module Spec.Control.Propagator.Scope.Arbitrary where

import "base" GHC.Exts ( IsList(..) )

import "tasty-quickcheck" Test.Tasty.QuickCheck

import "this" Control.Propagator.Scope
import "this" Spec.Data.Some.Arbitrary ()


instance Arbitrary Scope where
    arbitrary = fromList <$> arbitrary
