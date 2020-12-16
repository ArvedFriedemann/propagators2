module Terms where

import "this" Control.Propagator
import "this" Data.Facts

data TermConst = TOP | BOT | AND | OR | IMPL | CUST String | ID Int
  deriving (Show, Eq, Ord)
data Term a = CON TermConst
            | APPL a a
  deriving (Show, Eq, Ord)

data VarTerm t v = VVar v
                  | VTerm (t (VarTerm t v))
  deriving (Show, Eq, Ord)

type OpenVarTerm v = VarTerm Term v

unifyM :: (PropagatorMonad m) => OpenVarTerm v -> OpenVarTerm v -> m Bool
unifyM (VTerm (CON c1)) (VTerm (Con c2)) = return $ c1 == c2
unifyM (VVar v1) (VVAR v2)
  | v1 == v2 = return true
  | otherwise = ?
unifyM t (VVar v) = unifyM (VVar v) t
unifyM (VVar v) t = do
  t' <- readCell v
  unifyM t' t
unifyM (VTerm (APPL a1 b2)) (VTerm (a2 b2)) = (&&) <$> unifyM a1 a2 <$> unifyM b1 b2


unify :: (PropagatorMonad m) => Cell m (OpenVarTerm v) -> Cell m (OpenVarTerm v) -> Cell m Bool -> m (m ())
unify t1 t2 out = linkM t1 t2 unifyM
