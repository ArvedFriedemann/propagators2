{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Hashable.Orphans where

import "base" Data.Ord
import "base" Data.Monoid
import "base" Control.Applicative

import "hashable" Data.Hashable


deriving newtype instance Hashable a => Hashable (Down a)
deriving newtype instance Hashable a => Hashable (Product a)
deriving newtype instance Hashable a => Hashable (Sum a)
deriving newtype instance Hashable a => Hashable (Dual a)
deriving newtype instance Hashable a => Hashable (ZipList a)
deriving newtype instance Hashable a => Hashable (Last a)
deriving newtype instance Hashable a => Hashable (First a)
