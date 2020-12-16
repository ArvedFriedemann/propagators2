{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Data.Terms where

import "this" Control.Propagator
import "this" Data.Facts
import "this" Data.Constraints.Combinators
import "base" Control.Monad

data TermConst = TOP | BOT | AND | OR | IMPL | CUST String | ID Int
  deriving (Show, Eq, Ord)
data Term a = CON TermConst
            | APPL a a
  deriving (Show, Eq, Ord)

data VarTerm m t = VVar (Cell m (VarTerm m t))
                  | VTerm (t (VarTerm m t))


type OpenVarTerm m = VarTerm m Term

unifyM :: (PropagatorMonad m) => OpenVarTerm m -> OpenVarTerm m -> Cell m Bool -> m ()
unifyM (VTerm (CON c1)) (VTerm (CON c2)) out = write out $ c1 == c2
unifyM (VVar v1) (VVar v2) out
  | v1 == v2 = write out True
  | otherwise = void $ unifyTerms v1 v2 out
unifyM t (VVar v) out = unifyM (VVar v) t out
unifyM (VVar v) t out = do
  ct <- newCell t
  unifyTerms v ct out
  return ()
unifyM (VTerm (APPL a1 b1)) (VTerm (APPL a2 b2)) out = do
  out1 <- newEmptyCell
  out2 <- newEmptyCell
  unifyM a1 a2 out1
  unifyM b1 b2 out2
  conjunct out1 out2 out
  return ()


unifyTerms :: (PropagatorMonad m) => Cell m (OpenVarTerm m) -> Cell m (OpenVarTerm m) -> Cell m Bool -> m (m ())
unifyTerms t1 t2 out = linkM2 t1 t2 out unifyM
