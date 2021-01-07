{-# LANGUAGE UndecidableInstances #-}
module Data.Terms.Terms where

import "base" Data.Maybe
import "base" Data.Function
import "base" Control.Applicative
import "base" Control.Monad
import "base" GHC.Exts
import "base" Debug.Trace

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

--import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

import "this" Data.Lattice
import "this" Control.Propagator
--import "this" Control.Util


data TermConst
    = TOP
    | BOT
    | AND
    | OR
    | IMPL
    | CUST String
    | ID Int
  deriving (Show, Eq, Ord)
data Term a
    = Var a
    | Con TermConst
    | App a a
  deriving (Show, Eq, Ord, Functor, Foldable)


data TermSet a
    = TSBot
    | TS
        { constant :: Maybe TermConst
        , variables :: Set a
        , applications :: Set (a, a)
        }
  deriving (Eq, Ord, Show)

instance IsString TermConst where
  fromString = CUST

constTerm :: Ord a => TermConst -> TermSet a
constTerm c = top {constant = Just c}

varTerm :: Ord a => a -> TermSet a
varTerm v = top {variables = Set.singleton v}

aplTerm :: Ord a => (a, a) -> TermSet a
aplTerm app = top {applications = Set.singleton app}

liftTS2 :: Ord a => (forall b. Ord b => Set b -> Set b -> Set b) -> TermSet a -> TermSet a -> TermSet a
liftTS2 _ Bot _ = Bot
liftTS2 _ _ Bot = Bot
liftTS2 _ (TS (Just a) _ _) (TS (Just b) _ _) | a /= b = Bot
liftTS2 f a b = cleanTermSet TS
    { constant = on (<|>) constant a b
    , variables = on f variables a b
    , applications = on f applications a b
    }
  where
    cleanTermSet ts@(TS cs _ as)
        --cannot have two different constants
        | (on (&&) (isJust.constant) a b) &&
          (on (/=) (fromJust.constant) a b) = Bot
        --the value cannot be application and constant atst.
        | (not $ Set.null as) && isJust cs = Bot
        | otherwise = ts
    cleanTermSet _ = Bot

instance Ord a => Meet (TermSet a) where
    (/\) = liftTS2 Set.union
instance Ord a => BoundedMeet (TermSet a) where
    top = TS Nothing Set.empty Set.empty
instance Ord a => Join (TermSet a) where
    (\/) = liftTS2 Set.intersection
instance Ord a => BoundedJoin (TermSet a) where
    bot = TSBot
instance Ord a => Lattice (TermSet a)
instance Ord a => BoundedLattice (TermSet a)

propBot :: (MonadProp m, Identifier i b, BoundedJoin b, Eq a, BoundedJoin a)
        => i -> a -> m ()
propBot i a = when (a == Bot) . void $ write i Bot

watchTerm :: (Ord i, MonadProp m, Identifier i (TermSet i)) => i -> m i
watchTerm ct = watch ct ("term" :: String, ct) $ termListener ct

--WARNING: Does not remove listeners after join!
termListener :: forall i m. (Ord i, MonadProp m, Identifier i (TermSet i)) => i -> TermSet i -> m ()
termListener _ Bot = pure ()
termListener this ts = do
    --equality for variables
    eqAll . Set.insert this . variables $ ts
    --equality for applications
    let appList = Set.toList . applications $ ts
    eqAll $ fst <$> appList
    eqAll $ snd <$> appList
    --as subvalues are not equivalent to this value, their bots have to be propagated as well
    let propBotThis a = watch a ("propBot" :: String, this) $ propBot this
    mapM_ propBotThis $ fst <$> appList
    mapM_ propBotThis $ snd <$> appList


class CopyTermId w i | i -> w where
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
    traceM $ show tbl
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
refreshVarListener listId orig _ TSBot = void $ write (copy listId orig) bot
refreshVarListener listId orig trans (TS constants' variables' applications') = do

  watchTerm (copy listId orig)

  forM_ constants' $(\c -> do
      traceM $ "constant conversion: "++(show c)++ " goes to "++(show $ trans c)
      let mc = trans c in do
        case mc of
          Nothing -> write (copy listId orig)
                            (constTerm c)
          Just tc-> write (copy listId orig)
                            (varTerm tc)
          --important! The tc cannot be wrapped in a copy because it is not a copy!
    )

  forM_ variables' $(\v -> do
      write (copy listId orig)
        (varTerm (copy listId v))
      --TODO: it is correct to use the same listID here?
      --listeners are local, so it should not remove other listeners of the kind, but I am not sure.
      watch v listId (refreshVarListener listId v trans)
      --done in the upper listener
      --watchTerm (Copy listId v)
    )

  forM_ applications' $(\(a,b) -> do
      write (copy listId orig)
        (aplTerm (copy listId a, copy listId b))
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
refreshVarListener' _ orig _ TSBot = void $ write orig bot
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
              then void $ write orig (varTerm p')
              else pure ()
            _ -> pure ()
          Just c -> void $ write orig (constTerm c)
      --more cannot be deduced from variables as new ones might have been added after a unification. Only deducible variables are thos directly created by this constraint

    )
  --technically can't know stuff here either...except that there should be some application, but never its destinations. Only those specifically created by this copy listener could be deduced back.
  forM_ applications' $(\(a,b) -> do
      case (copyTermIdContents a,copyTermIdContents b) of
        (Just(listId', a'), Just(listId'', b')) ->
          if listId'  == listId &&
             listId'' == listId
          then do
            write orig (aplTerm (a', b'))
            watch a listId (refreshVarListener' listId a' trans)
            watch b listId (refreshVarListener' listId b' trans)
            pure ()
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
