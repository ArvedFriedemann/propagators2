{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.DFS where

import "this" Control.Combinator.Logics

import "base" Prelude hiding ( read )

{-}
dfsPromoterDestr :: (MonadProp m v scope, Value a, HasBot a, Std n, StdPtr v) => n -> v a -> m () -> [(m (), m ())] -> m ()
dfsPromoterDestr ctx goalVar finDestr [] = finDestr
dfsPromoterDestr ctx goalVar finDestr ms = do
-}
