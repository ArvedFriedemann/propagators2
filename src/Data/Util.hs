module Data.Util where

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

isSingleton :: Set a -> Bool
isSingleton s = case Set.toList s of
                  [_] -> True
                  _ -> False

greaterOne :: Set a -> Bool
greaterOne s = case Set.toList s of
                  (_:_:_) -> True
                  _ -> False

setAppend :: (Ord a) => a -> Set a -> Set a
setAppend x s = Set.union (Set.singleton x) s
