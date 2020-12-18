module Control.Propagator.Bool where

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Control.Propagator.Class

table3 :: ( PropagatorMonad m
          , Ord a
          , Ord b
          , Ord c
          )
        => Cell m (Set (a, b, c))
        -> Cell m (Set a) -> Cell m (Set b) -> Cell m (Set c)
        -> m ()
table3 ct ca cb cc = do
    watch ca $ \ a -> do
        t <- readCell ct
        let t' = Set.filter (\ (a', _, _) -> Set.member a' a) t
        write ct t'
    watch cb $ \ b -> do
        t <- readCell ct
        let t' = Set.filter (\ (_, b', _) -> Set.member b' b) t
        write ct t'
    watch cc $ \ c -> do
        t <- readCell ct
        let t' = Set.filter (\ (_, _, c') -> Set.member c' c) t
        write ct t'
    watch ct $ \ t -> do
        let a = Set.map (\ (x, _, _) -> x) t
        let b = Set.map (\ (_, x, _) -> x) t
        let c = Set.map (\ (_, _, x) -> x) t

        write ca a
        write cb b
        write cc c
    pure ()