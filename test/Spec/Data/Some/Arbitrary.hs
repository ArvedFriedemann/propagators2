{-# OPTIONS_GHC -fno-warn-orphans #-}
module Spec.Data.Some.Arbitrary where

import "tasty-quickcheck" Test.Tasty.QuickCheck

import "this" Data.Some
import "this" Control.Propagator.Class


instance Arbitrary (Some Std) where
    arbitrary = oneof
        [ Some <$> arbitrary @Int
        , Some <$> arbitrary @Bool
        , Some <$> arbitrary @()
        , Some <$> arbitrary @String
        ]
