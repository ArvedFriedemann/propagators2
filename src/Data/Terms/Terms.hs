{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo #-}
module Data.Terms.Terms where

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Data.Lattice
import "this" Control.Propagator


data TermConst
    = TOP
    | BOT
    | AND
    | OR
    | IMPL
    | CUST String
    | ID Int
    | CUSTOM String
  deriving (Show, Eq, Ord)
data Term a
    = Var a
    | Con TermConst
    | App a a
  deriving (Show, Eq, Ord, Functor, Foldable)

data OVTConstructor
    = OVTVar
    | OVTCon
    | OVTApp
  deriving (Show, Eq, Ord)

ovtToConstructor :: Term a -> OVTConstructor
ovtToConstructor (Var _) = OVTVar
ovtToConstructor (Con _) = OVTCon
ovtToConstructor (App _ _) = OVTApp

ovtIsVar ::  Term a -> Bool
ovtIsVar t = ovtToConstructor t == OVTVar

ovtIsCon ::  Term a -> Bool
ovtIsCon t = ovtToConstructor t == OVTCon

ovtIsApl ::  Term a -> Bool
ovtIsApl t = ovtToConstructor t == OVTApp

constantContent :: Term a -> [TermConst]
constantContent (Con c) = [c]
constantContent _ = []

constantContents :: [Term a] -> [TermConst]
constantContents ts = concatMap constantContent ts

variableContent :: Term a -> [a]
variableContent (Var v) = [v]
variableContent _ = []

variableContents :: [Term a] -> [a]
variableContents ts = concatMap variableContent ts

applContent :: Term a -> [(a, a)]
applContent (App a b) = [(a,b)]
applContent _ = []

applContents :: [Term a] -> [(a, a)]
applContents ts = concatMap applContent ts

applLefts :: [Term a] -> [a]
applLefts ts = fst <$> applContents ts

aplRights :: [Term a] -> [a]
aplRights ts = snd <$> applContents ts



data TermSet a
    = TSBot
    | TS
        { constants :: Set TermConst
        , variables :: Set a
        , applications :: Set (a, a)
        }
  deriving (Eq, Ord, Show)


emptyTermSet :: TermSet a
emptyTermSet = TS Set.empty Set.empty Set.empty

termSetWithConstants :: Set TermConst -> TermSet a
termSetWithConstants cvs = emptyTermSet {constants = cvs}

termSetWithVariables :: Set a -> TermSet a
termSetWithVariables cvs = emptyTermSet {variables = cvs}

termSetWithApls :: Set (a, a) -> TermSet a
termSetWithApls cvs = emptyTermSet {applications = cvs}

cleanTermSet :: TermSet a -> TermSet a
cleanTermSet TSBot = TSBot
cleanTermSet ts
    --the value cannot be application and constant atst.
    | (not $ Set.null (applications ts)) &&
      (not $ Set.null (constants ts)) = TSBot
    --no more than one constant allowed
    | length (constants ts) > 1 = TSBot
    | otherwise = ts

instance Ord a => Meet (TermSet a) where
    TSBot /\ _ = TSBot
    _ /\ TSBot = TSBot
    ts1 /\ ts2 = cleanTermSet $ TS {
        constants = Set.union (constants ts1) (constants ts2),
        variables = Set.union (variables ts1) (variables ts2),
        applications = Set.union (applications ts1) (applications ts2)
    }
instance Ord a => BoundedMeet (TermSet a) where
    top = emptyTermSet
instance Ord a => Join (TermSet a) where
    TSBot \/ a = a
    a \/ TSBot = a
    --WARNING: This still needs to store which listeners should be removed!
    ts1 \/ ts2 = cleanTermSet $ TS {
        constants = Set.intersection (constants ts1) (constants ts2),
        variables = Set.intersection (variables ts1) (variables ts2),
        applications = Set.intersection (applications ts1) (applications ts2)
    }
instance Ord a => BoundedJoin (TermSet a) where
    bot = TSBot
instance Ord a => Lattice (TermSet a)
instance Ord a => BoundedLattice (TermSet a)

propBot :: ( MonadProp m
           , Identifier i b, BoundedJoin b
           , Eq a, BoundedJoin a
           )
        => i -> a -> m ()
propBot i a
    | a == bot  = write i bot
    | otherwise = pure ()

watchTerm :: (Ord i, MonadProp m, Identifier i (TermSet i)) => i -> m ()
watchTerm ct = watch ct ("term" :: String, ct) $ termListener ct

--WARNING: Does not remove listeners after join!
termListener :: (Ord i, MonadProp m, Identifier i (TermSet i)) => i -> TermSet i -> m ()
termListener _ TSBot = pure ()
termListener this ts = do
    --equality for variables
    mapM_ (eq this) (Set.toList $ variables ts)
    let appList = Set.toList . applications $ ts
    --equality for applications
    eqAll $ fst <$> appList
    eqAll $ snd <$> appList
    --as subvalues are not equivalent to this value, their bots have to be propagated as well
    let propBotThis a = watch a ("propBot" :: String, a) $ propBot this
    mapM_ propBotThis $ fst <$> appList
    mapM_ propBotThis $ snd <$> appList
