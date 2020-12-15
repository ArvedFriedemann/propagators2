module Data.Facts where

import "containers" Data.Set ( Set )
-- import "containers" Data.Set qualified as Set

-- import "this" Data.Lattice


newtype Facts a = Facts (Set (Fact a))

class Ord (Fact a) => HasFacts a where
    data Fact a


