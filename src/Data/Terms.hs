{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Data.Terms where

import "this" Data.Lattice
import "this" Control.Propagator
import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as S

data TermConst = TOP | BOT | AND | OR | IMPL | CUST String | ID Int | CUSTOM String
  deriving (Show, Eq, Ord)
data Term a = CON TermConst
            | APPL a a
  deriving (Show, Eq, Ord)

data OpenVarTerm m = VVar (Cell m (TermSet m))
                  | VTerm (Term (OpenVarTerm m))
deriving instance (Show (Cell m (TermSet m)), Show (Term (OpenVarTerm m)) ) => Show (OpenVarTerm m)
deriving instance (Eq (Cell m (TermSet m)), Eq (Term (OpenVarTerm m)) ) => Eq (OpenVarTerm m)
deriving instance (Ord (Cell m (TermSet m)), Ord (Term (OpenVarTerm m)) ) => Ord (OpenVarTerm m)

data OVTConstructor = OVTVar | OVTCon | OVTAppl
  deriving (Show, Eq, Ord)

ovtToConstructor :: (PropagatorMonad m) => OpenVarTerm m -> OVTConstructor
ovtToConstructor (VVar _) = OVTVar
ovtToConstructor (VTerm (CON _)) = OVTCon
ovtToConstructor (VTerm (APPL _ _)) = OVTAppl

ovtIsVar ::  (PropagatorMonad m) => OpenVarTerm m -> Bool
ovtIsVar t = ovtToConstructor t == OVTVar
ovtIsCon ::  (PropagatorMonad m) => OpenVarTerm m -> Bool
ovtIsCon t = ovtToConstructor t == OVTCon
ovtIsApl ::  (PropagatorMonad m) => OpenVarTerm m -> Bool
ovtIsApl t = ovtToConstructor t == OVTAppl


data TermSet m =   TSBot
                 | TS (Set (OpenVarTerm m))
  --deriving (Show, Eq, Ord)
deriving instance (PropagatorMonad m) => Show (TermSet m)
deriving instance (PropagatorMonad m) => Eq (TermSet m)
deriving instance (PropagatorMonad m) => Ord (TermSet m)

cleanTermSet :: (PropagatorMonad m) => TermSet m -> TermSet m
cleanTermSet TSBot = TSBot
cleanTermSet (TS ts)
  | length (filter S.null [vars, apls, cnst]) > 1 = TSBot
  | otherwise = TS ts
  where vars = S.filter ovtIsVar ts
        apls = S.filter ovtIsApl ts
        cnst = S.filter ovtIsCon ts

instance (PropagatorMonad m) => Meet (TermSet m) where
  TSBot /\ _ = TSBot
  _ /\ TSBot = TSBot
  (TS a) /\ (TS b) = TS $ S.union a b
instance (PropagatorMonad m) => BoundedMeet (TermSet m) where
    top = TS $ S.empty
instance (PropagatorMonad m) => Join (TermSet m) where
  TSBot \/ a = a
  a \/ TSBot = a
  (TS a) \/ (TS b) = TS $ S.intersection a b
instance (PropagatorMonad m) => BoundedJoin (TermSet m) where
    bot = TSBot
instance (PropagatorMonad m) => Lattice (TermSet m)
instance (PropagatorMonad m) => BoundedLattice (TermSet m)



--termListener :: FactSet

{-
unifyM :: (PropagatorMonad m) =>
  (Cell m (Fact (OpenVarTerm m)), Fact (OpenVarTerm m)) ->
  (Cell m (Fact (OpenVarTerm m)), Fact (OpenVarTerm m)) ->
          (Cell m (Fact Bool), Fact Bool) -> m ()
unifyM (_, Bot) (ct2, _) (cb, _) = do
  write ct2 Bot
  write cb  Bot
unifyM (ct1, _) (_, Bot) (cb, _) = do
  write ct1 Bot
  write cb  Bot
unifyM (ct1, _) (ct2, _) (_, Bot) = do
  write ct1 Bot
  write ct2 Bot
unifyM (ct1, Top) (ct2, t2) (cb, b) = unifyM (ct2, t2) (ct1, Top) (cb, b)
unifyM (_, t1) (ct2, Top) (_, Atom True) = write ct2 t1
unifyM (ct1, )


unifyMTTBFact :: (PropagatorMonad m) => Fact (OpenVarTerm m) -> Fact (OpenVarTerm m) -> m (Set Bool)
unifyMTTBFact (Atom t1) (Atom t2) = unifyMTTB t1 t2
unifyMTTBFact Bot _ = return $ Set.empty
unifyMTTBFact _ Bot = return $ Set.empty
unifyMTTBFact _ _ = return $ Set.fromList [True,False]

unifyMTTB :: (PropagatorMonad m) => OpenVarTerm m -> OpenVarTerm m -> m (Set Bool)
unifyMTTB (VTerm (CON c1)) (VTerm (CON c2)) = return $ Set.fromList [c1 == c2]
unifyMTTB (VVar v1) (VVar v2)
  | v1 == v2 = return $ Set.fromList [True]
  | otherwise = return $ Set.fromList [True, False]
unifyMTTB (VVar v) t = do
  t' <- readCell v
  unifyMTTBFact t' (Atom t)
unifyMTTB t (VVar v) = do
  t' <- readCell v
  unifyMTTBFact (Atom t) t'
unifyMTTB (VTerm (APPL a1 b1)) (VTerm (APPL a2 b2)) = do
  unifyMTTB a1 a2
  unifyMTTB b1 b2
unifyMTTB _ _ = return $ Set.empty
-}


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
