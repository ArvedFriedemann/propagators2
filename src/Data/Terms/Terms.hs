{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo #-}
module Data.Terms.Terms where

import "base" GHC.Generics hiding (to, from)
import "base" Control.Monad

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as S

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as M

import "base" Data.Maybe

import "deepseq" Control.DeepSeq

import "this" Data.Lattice
import "this" Control.Propagator
import "this" Control.Util


data TermConst = TOP | BOT | AND | OR | IMPL | CUST String | ID Int | CUSTOM String
  deriving (Show, Eq, Ord, Generic)

data Term a = CON TermConst
            | APPL a a
  deriving (Show, Eq, Ord, Functor, Foldable, Generic)

data OpenVarTerm m = VVar (Cell m (TermSet m))
                  | VTerm (Term (Cell m (TermSet m)))
  deriving Generic
deriving instance PropagatorMonad m => Show (OpenVarTerm m)
deriving instance PropagatorMonad m => Eq (OpenVarTerm m)
deriving instance PropagatorMonad m => Ord (OpenVarTerm m)


data OVTConstructor = OVTVar | OVTCon | OVTAppl
  deriving (Show, Eq, Ord, Generic)
instance NFData OVTConstructor

ovtToConstructor :: PropagatorMonad m => OpenVarTerm m -> OVTConstructor
ovtToConstructor (VVar _) = OVTVar
ovtToConstructor (VTerm (CON _)) = OVTCon
ovtToConstructor (VTerm (APPL _ _)) = OVTAppl

ovtIsVar ::  PropagatorMonad m => OpenVarTerm m -> Bool
ovtIsVar t = ovtToConstructor t == OVTVar
ovtIsCon ::  PropagatorMonad m => OpenVarTerm m -> Bool
ovtIsCon t = ovtToConstructor t == OVTCon
ovtIsApl ::  PropagatorMonad m => OpenVarTerm m -> Bool
ovtIsApl t = ovtToConstructor t == OVTAppl

constantContent :: PropagatorMonad m => OpenVarTerm m -> [TermConst]
constantContent (VTerm (CON c)) = [c]
constantContent _ = []

constantContents :: PropagatorMonad m => [OpenVarTerm m] -> [TermConst]
constantContents ts = concatMap constantContent ts

variableContent :: PropagatorMonad m => OpenVarTerm m -> [Cell m (TermSet m)]
variableContent (VVar v) = [v]
variableContent _ = []

variableContents :: PropagatorMonad m => [OpenVarTerm m] -> [Cell m (TermSet m)]
variableContents ts = concatMap variableContent ts

applContent :: PropagatorMonad m => OpenVarTerm m -> [(Cell m (TermSet m),Cell m (TermSet m))]
applContent (VTerm (APPL a b)) = [(a,b)]
applContent _ = []

applContents :: PropagatorMonad m => [OpenVarTerm m] -> [(Cell m (TermSet m),Cell m (TermSet m))]
applContents ts = concatMap applContent ts

applLefts :: PropagatorMonad m => [OpenVarTerm m] -> [Cell m (TermSet m)]
applLefts ts = fst <$> applContents ts

aplRights :: PropagatorMonad m => [OpenVarTerm m] -> [Cell m (TermSet m)]
aplRights ts = snd <$> applContents ts



data TermSet m =   TSBot
                 | TS {
                   constants :: Set (OpenVarTerm m),
                   variables :: Set (OpenVarTerm m),
                   applications :: Set (OpenVarTerm m)
                 }
  deriving Generic
deriving instance PropagatorMonad m => Show (TermSet m)
deriving instance PropagatorMonad m => Eq (TermSet m)
deriving instance PropagatorMonad m => Ord (TermSet m)

emptyTermSet :: PropagatorMonad m => TermSet m
emptyTermSet = TS (S.empty) (S.empty) (S.empty)

termSetWithConstants :: PropagatorMonad m => (Set (OpenVarTerm m)) -> TermSet m
termSetWithConstants cvs = TS {
  constants = cvs,
  variables = S.empty,
  applications = S.empty
}

termSetWithVariables :: PropagatorMonad m => (Set (OpenVarTerm m)) -> TermSet m
termSetWithVariables cvs = TS {
  constants = S.empty,
  variables = cvs,
  applications = S.empty
}

termSetWithApls :: PropagatorMonad m => (Set (OpenVarTerm m)) -> TermSet m
termSetWithApls cvs = TS {
  constants = S.empty,
  variables = S.empty,
  applications = cvs
}

cleanTermSet :: PropagatorMonad m => TermSet m -> TermSet m
cleanTermSet TSBot = TSBot
cleanTermSet ts
  --the value cannot be application and constant atst.
  | (not $ S.null (applications ts)) &&
    (not $ S.null (constants ts)) = TSBot
  --no more than one constant allowed
  | length (constants ts) > 1 = TSBot
  | otherwise = ts

instance PropagatorMonad m => Meet (TermSet m) where
  TSBot /\ _ = TSBot
  _ /\ TSBot = TSBot
  (ts1) /\ (ts2) = cleanTermSet $ TS {
    constants = S.union (constants ts1) (constants ts2),
    variables = S.union (variables ts1) (variables ts2),
    applications = S.union (applications ts1) (applications ts2)
  }
instance PropagatorMonad m => BoundedMeet (TermSet m) where
    top = emptyTermSet
instance PropagatorMonad m => Join (TermSet m) where
  TSBot \/ a = a
  a \/ TSBot = a
  --WARNING: This still needs to store which listeners should be removed!
  ts1 \/ ts2 = cleanTermSet $ TS {
    constants = S.intersection (constants ts1) (constants ts2),
    variables = S.intersection (variables ts1) (variables ts2),
    applications = S.intersection (applications ts1) (applications ts2)
  }
instance PropagatorMonad m => BoundedJoin (TermSet m) where
    bot = TSBot
instance PropagatorMonad m => Lattice (TermSet m)
instance PropagatorMonad m => BoundedLattice (TermSet m)

propBot :: (Value b, Value a, Applicative m, PropagatorMonad m, BoundedJoin a, BoundedJoin b) =>
            Cell m b -> a -> m ()
propBot cout cin = do
  if cin == bot
    then write cout bot
    else pure ()

watchTerm :: (Applicative m, PropagatorMonad m) => Cell m (TermSet m) -> m (Subscriptions m)
watchTerm ct = namedWatch ct "term" $ termListener ct

--WARNING: Does not remove listeners after join!
termListener :: (Applicative m, PropagatorMonad m) => Cell m (TermSet m) -> TermSet m -> m ()
termListener _ TSBot = pure ()
termListener this ts = do
  --equality for variables
  traverse (eq this) (variableContents (S.toList $ variables ts))
  --equality for applications
  eqAll $ applLefts (S.toList (applications ts))
  eqAll $ aplRights (S.toList (applications ts))
  --as subvalues are not equivalent to this value, their bots have to be propagated as well
  let propBotThis = flip watch $ propBot this
  let appList = S.toList . applications $ ts
  traverse propBotThis $ applLefts appList
  traverse propBotThis $ aplRights appList

  return ()

refreshVarsTbl :: forall m. (PropagatorMonad m) =>
  [(TermConst, Cell m (TermSet m))] ->
  (Cell m (TermSet m)) -> (Cell m (TermSet m)) -> m ()
refreshVarsTbl tbl = refreshVars to from
  where
    mto = M.fromList tbl
    mfrom = M.fromList ((\(x,y) -> (y,x)) <$> tbl)
    to :: TermConst -> [Cell m (TermSet m)]
    to = maybeToList. flip M.lookup mto
    from :: Cell m (TermSet m) -> [TermConst]
    from = maybeToList . flip M.lookup mfrom

refreshVars :: forall m. (PropagatorMonad m) =>
  (TermConst -> [Cell m (TermSet m)]) ->
  (Cell m (TermSet m) -> [TermConst]) ->
  (Cell m (TermSet m)) -> (Cell m (TermSet m)) -> m ()
refreshVars to from orig copy = void $ do
    watch orig (copyTermListener to' copy)
    watch copy (copyTermListener from' orig)
  where
    to' :: TermSet m -> TermSet m
    to' ts = termSetWithVariables (S.fromList $ VVar <$> concatMap to (constantContents (S.toList $ constants ts)) )
    from' :: TermSet m -> TermSet m
    from' ts = termSetWithVariables (S.fromList $ (VTerm . CON) <$> concatMap from (variableContents (S.toList $ variables ts)) )

copyTermListener :: (PropagatorMonad m) =>
  (TermSet m -> TermSet m) ->
  (Cell m (TermSet m)) -> TermSet m -> m ()
copyTermListener trans ccell orig = do

  write ccell (trans orig)

  unless (null $ applications orig) $ do
    let (VTerm (APPL v1 v2)) = S.findMin $ applications orig
      in void $ do
        v1' <- newEmptyCell "cpy1"
        v2' <- newEmptyCell "cpy2"
        write ccell (termSetWithApls $ S.singleton (VTerm (APPL v1' v2')))
        watch v1 (copyTermListener trans v1')
        watch v2 (copyTermListener trans v2')

  when ((null $ constants orig) &&
          (null $ applications orig) &&
          (not $ null $ variables orig)) $ do
    let (VVar v) = S.findMin $ variables orig
      in void $ do
        watch v (copyTermListener trans ccell) --WARNING: This spans infinitely many listeners unless no double listeners can be placed!

{-
unifyM :: PropagatorMonad m =>
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


unifyMTTBFact :: PropagatorMonad m => Fact (OpenVarTerm m) -> Fact (OpenVarTerm m) -> m (Set Bool)
unifyMTTBFact (Atom t1) (Atom t2) = unifyMTTB t1 t2
unifyMTTBFact Bot _ = return $ Set.empty
unifyMTTBFact _ Bot = return $ Set.empty
unifyMTTBFact _ _ = return $ Set.fromList [True,False]

unifyMTTB :: PropagatorMonad m => OpenVarTerm m -> OpenVarTerm m -> m (Set Bool)
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
unifyM :: PropagatorMonad m => Cell m Bool -> OpenVarTerm m -> OpenVarTerm m -> m ()
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


unifyTerms :: PropagatorMonad m => Cell m (OpenVarTerm m) -> Cell m (OpenVarTerm m) -> Cell m Bool -> m (m ())
unifyTerms t1 t2 out = linkM2 t1 t2 (unifyM out)
-}
