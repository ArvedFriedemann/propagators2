{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo #-}
module Data.Terms.Terms where

import "base" Control.Monad

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Data.Lattice
import "this" Control.Propagator
import "this" Control.Util


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

data CpyTermId w p =
    Direct p
  | Copy w p --id of the watch and the original
  deriving (Eq, Ord, Show)
instance (Ord p, Std p, Ord w, Std w) => Identifier (CpyTermId w p) (TermSet (CpyTermId w p))

refreshVarListener :: (Ord i, MonadProp m, Identifier i (TermSet i), Std w) => w -> i -> TermSet i -> m ()
refreshVarListener listId orig TSBot = write (Copy listId orig) bot
refreshVarListener listId orig (TS constants' variables' applications') = do

  --TODO: transfer constants

  forM_ variables' $(\v -> do
      write (Copy listId orig)
        (termSetWithVariables $ Set.singleton (Copy listId v))
      --TODO: it is correct to use the same listID here?
      --listeners are local, so it should not remove other listeners of the kind, but I am not sure.
      watch v listId (refreshVarListener listId v)
    )

  forM_ applications' $(\(a,b) -> do
      write (Copy listId orig)
        (termSetWithApls $ Set.singleton (Copy listId a, Copy listId b))
      watch a listId (refreshVarListener listId a)
      watch b listId (refreshVarListener listId b)
    )


{-
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
    placeCopyTermListener to' orig copy
    placeCopyTermListener from' copy orig
  where
    to' :: TermSet m -> TermSet m
    to' ts = emptyTermSet {
      constants' = S.fromList $ (VTerm . CON) <$> (filter (\c -> null $ to c) $ constantContents (S.toList $ constants ts) ) ,
      variables' = (S.fromList $ VVar <$> concatMap to (constantContents (S.toList $ constants ts)) )
    }
    from' :: TermSet m -> TermSet m
    from' ts = emptyTermSet {
      constants' = --S.union
        (S.fromList $ (VTerm . CON) <$> concatMap from  (variableContents (S.toList $ variables ts)))
        --This does not work because constants can come from somewhere outside the copy. In that case, they are pushed back into the old term, where the refreshed constant might lay, causing a conflict. Therefore, the constants cannot be propagated backwards.
        --(S.fromList $ (VTerm . CON) <$> (filter (\c -> null $ to c) $ constantContents (S.toList $ constants ts) ) )
    }

placeCopyTermListener :: (PropagatorMonad m) =>
  (TermSet m -> TermSet m) ->
  (Cell m (TermSet m)) -> (Cell m (TermSet m)) ->  m ()
placeCopyTermListener trans orig ccell = void $ do
  v1' <- newEmptyCell "cpy1" <**< watchTerm
  v2' <- newEmptyCell "cpy2" <**< watchTerm
  watch orig (copyTermListener v1' v2' trans ccell)

copyTermListener :: (PropagatorMonad m) =>
  Cell m (TermSet m) -> Cell m (TermSet m) ->
  (TermSet m -> TermSet m) ->
  (Cell m (TermSet m)) -> TermSet m -> m ()
copyTermListener v1' v2' trans ccell orig = do

  write ccell (trans orig)

  unless (null $ applications orig) $ do
    let (VTerm (APPL v1 v2)) = S.findMin $ applications orig
      in void $ do
        write ccell (termSetWithApls $ S.singleton (VTerm (APPL v1' v2')))
        placeCopyTermListener trans v1 v1'
        placeCopyTermListener trans v2 v2'
-}
