{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.Logics where

import "base" Prelude hiding ( read )
import "base" Control.Monad
import "base" Data.Maybe
import "base" Debug.Trace

import "this" Control.Propagator.Class
import "this" Control.Combinator.Combinators
import "this" Data.Lattice
import "this" Data.Some

data ScopeIdx a = ScopeIdx (Some Std) Int
  deriving (Show, Eq, Ord)
instance Identifier (ScopeIdx a) a

class Promoter a m where
  promoteAction :: a -> m ()






--
