{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo #-}
module Data.Terms.Terms where

import "base" Control.Monad

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

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


class CopyTermId w i where
  --copy listId origTerm
  copy :: w -> i -> i
  copyTermIdContents :: i -> Maybe (w,i)

refreshVarsTbl ::
  ( Ord i
  , MonadProp m
  , Identifier i (TermSet i)
  , CopyTermId w i
  , Std w) =>
  w -> [(TermConst, i)] -> i -> m i
refreshVarsTbl listId tbl orig = do
    watch orig listId (refreshVarListener listId orig trans)
    return (copy listId orig)
  where trans = flip Map.lookup (Map.fromList tbl)

refreshVarListener ::
  (Ord i
  , MonadProp m
  , Identifier i (TermSet i)
  , CopyTermId w i
  , Std w) =>
  w -> i -> (TermConst -> Maybe i) -> TermSet i -> m ()
refreshVarListener listId orig _ TSBot = write (copy listId orig) bot
refreshVarListener listId orig trans (TS constants' variables' applications') = do

  watchTerm (copy listId orig)

  forM_ constants' $(\c ->
      let mc = trans c in do
        case mc of
          Nothing -> write (copy listId orig)
            (termSetWithConstants $ Set.singleton c)
          Just tc-> write (copy listId orig)
            (termSetWithVariables $ Set.singleton tc)
          --important! The tc cannot be wrapped in a copy because it is not a copy!
    )

  forM_ variables' $(\v -> do
      write (copy listId orig)
        (termSetWithVariables $ Set.singleton (copy listId v))
      --TODO: it is correct to use the same listID here?
      --listeners are local, so it should not remove other listeners of the kind, but I am not sure.
      watch v listId (refreshVarListener listId v trans)
      --done in the upper listener
      --watchTerm (Copy listId v)
    )

  forM_ applications' $(\(a,b) -> do
      write (copy listId orig)
        (termSetWithApls $ Set.singleton (copy listId a, copy listId b))
      watch a listId (refreshVarListener listId a trans)
      watch b listId (refreshVarListener listId b trans)
      --done in the upper listener
      --watchTerm (Copy listId a)
      --watchTerm (Copy listId b)
    )


--TODO: Technically this thing is BS. The only values that could be retrieved were the ones specifically implanted by the first listener. However, in the general use case, these are never created by other listeners, wherefore the creation direction really is always one directional. As an example: The original term could just be one constant creating a variable. The created variable could be merged with any term, wherefore the resulted term does not contain any information about its origin except that it exists.
refreshVarListener' ::
  (Ord i
  , MonadProp m
  , Identifier i (TermSet i)
  , CopyTermId w i
  , Std w) =>
  w -> i -> (i -> Maybe TermConst) -> TermSet i -> m ()
refreshVarListener' _ orig _ TSBot = write orig bot
refreshVarListener' listId orig trans (TS _ variables' applications') = do

  watchTerm orig

  --can't deduce anything from the pure constants as they do not carry the copy reason with them

  forM_ variables' $(\v ->
      --this should check whether the thing is a "Direct" id first. Grrr...this smells of casting
      let mv = trans v in do
        case mv of
          Nothing -> case copyTermIdContents v of
            Just (listId', p') ->
              if listId == listId'
              then write orig
                (termSetWithVariables $ Set.singleton p')
              else pure ()
            _ -> pure ()
          Just c -> write orig
            (termSetWithConstants $ Set.singleton c)
      --more cannot be deduced from variables as new ones might have been added after a unification. Only deducible variables are thos directly created by this constraint

    )
  --technically can't know stuff here either...except that there should be some application, but never its destinations. Only those specifically created by this copy listener could be deduced back.
  forM_ applications' $(\(a,b) -> do
      case (copyTermIdContents a,copyTermIdContents b) of
        (Just(listId', a'), Just(listId'', b')) ->
          if listId'  == listId &&
             listId'' == listId
          then do
            write orig (termSetWithApls $ Set.singleton (a', b'))
            watch a listId (refreshVarListener' listId a' trans)
            watch b listId (refreshVarListener' listId b' trans)
          else pure ()
        _ -> pure ()
      --watchTerm (Copy listId a)
      --watchTerm (Copy listId b)
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
