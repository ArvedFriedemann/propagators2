module Data.Matrix
    ( Matrix(..)
    , cartesianProduct
    , transpose
    ) where

import "base" GHC.TypeNats
import "this" Data.Vec


newtype Matrix n m a = Matrix (Vec n (Vec m a))

cartesianProduct :: Vec i a -> Vec j b -> Matrix i j (a, b)
cartesianProduct as bs = Matrix $ fmap (flip fmap bs . (,)) as

transpose :: (KnownNat n, KnownNat m)
          => Matrix m n a -> Matrix n m a
transpose (Matrix n) = Matrix (sequenceA n)
