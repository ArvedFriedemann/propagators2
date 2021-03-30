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

{-
assumption: once two elements are equal, all following elements are equal
returns: (initPath a, initPath b, commonTail a b)
-}
longestCommonTail :: (Eq a, Ord a) => [a] -> [a] -> ([a],[a],[a])
longestCommonTail a b = (pa,pb,tl)
  where (pb,tl) = longestCommonTail' (Set.fromList a) b
        (pa,_ ) = longestCommonTail' (Set.fromList b) a

longestCommonTail' :: (Eq a, Ord a) => Set a -> [a] -> ([a],[a])
longestCommonTail' a = span (not . flip Set.member a)

split:: [a] -> (a,[a])
split [] = error "Splitting empty list!"
split (x:xs) = (x,xs)






  ---
