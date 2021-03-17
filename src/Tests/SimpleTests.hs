{-# LANGUAGE NoImplicitPrelude #-}
module Tests.SimpleTests where

import "base" Prelude hiding ( read )
import "base" Data.Typeable
import "base" Debug.Trace

import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Some

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

data FactSet a = FS (Set a) | FSBot
  deriving (Show, Eq, Ord, Typeable)
instance (Ord a) => Meet (FactSet a) where
  FSBot /\ _ = FSBot
  _ /\ FSBot = FSBot
  (FS a) /\ (FS b) = FS $ Set.union a b
instance (Ord a) => Join (FactSet a) where
  FSBot \/ a = a
  a \/ FSBot = a
  (FS a) \/ (FS b) = FS $ Set.intersection a b
instance (Eq a) => HasTop (FactSet a) where
  top = FS Set.empty
instance (Eq a) => HasBot (FactSet a) where
  bot = FSBot

data FactSetPointer a = FSP (Some Std)
  deriving (Show, Eq, Ord, Typeable)
instance Identifier (FactSetPointer a) (FactSet a)


data MonadPointer m = MonadPointer (Some Std)
  deriving (Show, Eq, Ord)
instance Identifier (MonadPointer m) (m ())

test :: forall m v scope. (MonadProp m v scope, Typeable m) => m String
test = do
  a <- new (FSP @String $ Some ("a" :: String))
  b <- new (FSP @String $ Some ("b" :: String))
  write a (FS $ Set.singleton ("Test" :: String))
  watch a (MonadPointer @m (Some ("dirEq" :: String))) (read a >>= write b)
  watch a (MonadPointer @m (Some ("trace" :: String)))
    (read a >>= \a'-> traceM $ "A is " ++ show a')
  return "succeeded"
