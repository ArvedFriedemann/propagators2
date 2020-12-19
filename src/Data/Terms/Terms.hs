{-# LANGUAGE UndecidableInstances #-}

module Data.Terms.Terms where

import "this" Data.Lattice
import "this" Control.Propagator
import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as S
--import "transformers" Control.Monad.Trans.Writer.Lazy
--import "base" Debug.Trace

data TermConst = TOP | BOT | AND | OR | IMPL | CUST String | ID Int | CUSTOM String
  deriving (Show, Eq, Ord)
data Term a = CON TermConst
            | APPL a a
  deriving (Show, Eq, Ord, Functor, Foldable)

data OpenVarTerm m = VVar (Cell m (TermSet m))
                  | VTerm (Term (Cell m (TermSet m)))
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

variableContent :: (PropagatorMonad m) => OpenVarTerm m -> [Cell m (TermSet m)]
variableContent (VVar v) = [v]
variableContent _ = []

variableContents :: (PropagatorMonad m) => [OpenVarTerm m] -> [Cell m (TermSet m)]
variableContents ts = concatMap variableContent ts

applContent :: (PropagatorMonad m) => OpenVarTerm m -> [(Cell m (TermSet m),Cell m (TermSet m))]
applContent (VTerm (APPL a b)) = [(a,b)]
applContent _ = []

applContents :: (PropagatorMonad m) => [OpenVarTerm m] -> [(Cell m (TermSet m),Cell m (TermSet m))]
applContents ts = concatMap applContent ts

applLefts :: (PropagatorMonad m) => [OpenVarTerm m] -> [Cell m (TermSet m)]
applLefts ts = fst <$> applContents ts

aplRights :: (PropagatorMonad m) => [OpenVarTerm m] -> [Cell m (TermSet m)]
aplRights ts = snd <$> applContents ts


data TermSet m =   TSBot
                 | TS (Set (OpenVarTerm m))
  --deriving (Show, Eq, Ord)
deriving instance (PropagatorMonad m) => Show (TermSet m)
deriving instance (PropagatorMonad m) => Eq (TermSet m)
deriving instance (PropagatorMonad m) => Ord (TermSet m)

cleanTermSet :: (PropagatorMonad m) => TermSet m -> TermSet m
cleanTermSet TSBot = TSBot
cleanTermSet (TS ts)
  --the value cannot be application and constant atst.
  | (not $ S.null apls) && (not $ S.null cnst) = TSBot
  --no more than one constant allowed
  | length cnst > 1 = TSBot
  | otherwise = TS ts
  where apls = S.filter ovtIsApl ts
        cnst = S.filter ovtIsCon ts

instance (PropagatorMonad m) => Meet (TermSet m) where
  TSBot /\ _ = TSBot
  _ /\ TSBot = TSBot
  (TS a) /\ (TS b) = cleanTermSet $ TS $ S.union a b
instance (PropagatorMonad m) => BoundedMeet (TermSet m) where
    top = TS $ S.empty
instance (PropagatorMonad m) => Join (TermSet m) where
  TSBot \/ a = a
  a \/ TSBot = a
  (TS a) \/ (TS b) = cleanTermSet $ TS $ S.intersection a b
instance (PropagatorMonad m) => BoundedJoin (TermSet m) where
    bot = TSBot
instance (PropagatorMonad m) => Lattice (TermSet m)
instance (PropagatorMonad m) => BoundedLattice (TermSet m)

propBot :: (PropagatorEqMonad m, Meet a, Ord a, BoundedJoin a, Meet b, Ord b, BoundedJoin b) =>
            Cell m b -> a -> m ()
propBot cout cin = do
  if cin == bot
    then write cout bot
    else return ()

watchTerm :: (PropagatorEqMonad m) => Cell m (TermSet m) -> m (Subscription m)
watchTerm ct = watch ct $ termListener ct

termListener :: (PropagatorEqMonad m) => Cell m (TermSet m) -> TermSet m -> m ()
termListener _ TSBot = return ()
termListener this (TS ts) = do
  --equality for variables
  mapM_ (eq this) varconts
  --equality for applications
  eqAll $ applLefts apls
  eqAll $ aplRights apls
  --as subvalues are not equivalent to this value, their bots have to be propagated as well
  mapM_ (\x -> watch x (propBot this)) $ applLefts apls
  mapM_ (\x -> watch x (propBot this)) $ aplRights apls
  return ()
  where varconts = variableContents (S.toList ts)
        apls = S.toList $ S.filter ovtIsApl ts

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