{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Data.Terms where

import "this" Control.Propagator

data TermConst = TOP | BOT | AND | OR | IMPL | CUST String | ID Int
  deriving (Show, Eq, Ord)
data Term a = CON TermConst
            | APPL a a
  deriving (Show, Eq, Ord)

data VarTerm m t = VVar (Cell m (VarTerm m t))
                  | VTerm (t (VarTerm m t))


type OpenVarTerm m = VarTerm m Term
{-
unifyM :: (PropagatorMonad m) => Cell m Bool -> OpenVarTerm m -> OpenVarTerm m -> m ()
unifyM out (VTerm (CON c1)) (VTerm (CON c2)) = write out $ c1 == c2
unifyM out (VVar v1) (VVar v2)
  | v1 == v2 = write out True
  | otherwise = void $ unifyTerms v1 v2 out
unifyM out t (VVar v) = unifyM out (VVar v) t
unifyM out (VVar v) t = do
  ct <- newCell t
  unifyTerms v ct out
  return ()
unifyM out (VTerm (APPL a1 b1)) (VTerm (APPL a2 b2)) = do
  out1 <- newEmptyCell
  out2 <- newEmptyCell
  ca1 <- newCell a1
  cb1 <- newCell b1
  ca2 <- newCell a2
  cb2 <- newCell b2
  unifyTerms ca1 ca2 out1
  unifyTerms cb1 cb2 out2
  conjunct out1 out2 out
  return ()


unifyTerms :: (PropagatorMonad m) => Cell m (OpenVarTerm m) -> Cell m (OpenVarTerm m) -> Cell m Bool -> m (m ())
unifyTerms t1 t2 out = linkM2 t1 t2 (unifyM out)
-}
