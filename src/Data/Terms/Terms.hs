{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
module Data.Terms.Terms where

import "base" Prelude hiding ( read )
import "base" Data.Maybe
import "base" Data.Function
import "base" Control.Applicative
import "base" Control.Monad
import "base" GHC.Exts
import "base" GHC.Generics

import "unordered-containers" Data.HashSet ( HashSet )
import "unordered-containers" Data.HashSet qualified as HashSet

import "unordered-containers" Data.HashMap.Strict ( HashMap )
import "unordered-containers" Data.HashMap.Strict qualified as HashMap

import "hashable" Data.Hashable

import "this" Data.Lattice
import "this" Data.Typed
import "this" Control.Propagator
import "this" Data.Some


data TermConst
    = TOP
    | BOT
    | AND
    | OR
    | IMPL
    | CUST String
    | GEN (Some Std)
    | ID Int
  deriving (Show, Eq, Ord, Generic)
instance Hashable TermConst
  {-}
data Term a
    = Var a
    | Con TermConst
    | App a a
  deriving (Show, Eq, Ord, Functor, Foldable)
-}

data TermSet a
    = TSBot
    | TS
        { constant :: Maybe TermConst
        , variables :: HashSet a
        , applications :: HashSet (a, a)
        }
  deriving (Eq, Ord, Show, Generic)
{-# COMPLETE Bot, TS #-}

instance Hashable a => Hashable (TermSet a)

instance IsString TermConst where
    fromString = CUST
instance IsString (TermSet a) where
    fromString = constTerm . CUST

constTerm :: TermConst -> TermSet a
constTerm c = top {constant = Just c}

varTerm :: Hashable a => a -> TermSet a
varTerm v = top {variables = HashSet.singleton v}

aplTerm :: Hashable a => (a, a) -> TermSet a
aplTerm app = top {applications = HashSet.singleton app}

liftTS2 :: (Eq a, Hashable a) => (forall b. (Eq b, Hashable b) => HashSet b -> HashSet b -> HashSet b) -> TermSet a -> TermSet a -> TermSet a
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
        | (not $ HashSet.null as) && isJust cs = Bot
        | otherwise = ts
    cleanTermSet _ = Bot

instance (Eq a, Hashable a) => Meet (TermSet a) where
    (/\) = liftTS2 HashSet.union
instance HasTop (TermSet a) where
    isTop (TS Nothing v a) = HashSet.null v && HashSet.null a
    isTop _ = False
    top = TS Nothing HashSet.empty HashSet.empty
instance (Eq a, Hashable a) => Join (TermSet a) where
    (\/) = liftTS2 HashSet.intersection
instance HasBot (TermSet a) where
    isBot TSBot = True
    isBot _ = False
    bot = TSBot
instance (Eq a, Hashable a) => BoundedMeet (TermSet a)
instance (Eq a, Hashable a) => BoundedJoin (TermSet a)
instance (Eq a, Hashable a) => Lattice (TermSet a)
instance (Eq a, Hashable a) => BoundedLattice (TermSet a)


watchTerm :: (Ord i, MonadProp m, Identifier i (TermSet i)) => i -> m i
watchTerm ct = watch ct $ TermListener ct

newtype TermListener i = TermListener i deriving (Eq, Ord, Show, Generic, Hashable)

instance (Identifier i (TermSet i), MonadProp m) => Propagator m  (TermSet i) (TermListener i) where
    --WARNING: Does not remove listeners after join!
    propagate _ Bot = pure ()
    propagate (TermListener this) ts = do
        --equality for variables
        eqAll . HashSet.insert this . variables $ ts
        --equality for applications
        let appList = HashSet.toList . applications $ ts
        eqAll $ fst <$> appList
        eqAll $ snd <$> appList

        --sequence_ $ read . fst <$> appList
        --sequence_ $ read . snd <$> appList
        --as subvalues are not equivalent to this value, their bots have to be propagated as well
        let propBotThis a = propBot a this
        mapM_ propBotThis $ fst <$> appList
        mapM_ propBotThis $ snd <$> appList


promoteTerm :: (Ord i, MonadProp m, Identifier i (TermSet i)) =>
                i -> m i
promoteTerm t = watch t $ TermPromoter t

newtype TermPromoter i = TermPromoter i deriving (Eq, Ord, Show, Generic, Hashable)

instance (Identifier i (TermSet i), MonadProp m) => Propagator m  (TermSet i) (TermPromoter i) where
    propagate (TermPromoter this) Bot = promote this
    propagate (TermPromoter this) ts = do
      promote this
      forM_ (variables ts) $ \v -> do
        promoteTerm v
      forM_ (applications ts) $ \(a,b) -> do
        promoteTerm a
        promoteTerm b

requestTerm :: (Ord i, MonadProp m, Identifier i (TermSet i)) =>
                i -> m i
requestTerm t = watch t $ TermRequester t

newtype TermRequester i = TermRequester i deriving (Eq, Ord, Show, Generic, Hashable)

instance (Identifier i (TermSet i), MonadProp m) => Propagator m  (TermSet i) (TermRequester i) where
    --WARNING: Does not remove listeners after join!
    propagate _ Bot = pure ()
    propagate (TermRequester this) ts = do
      request this
      forM_ (variables ts) $ \v -> do
        watch v $ TermRequester v
      forM_ (applications ts) $ \(a,b) -> do
        watch a $ TermRequester a
        watch b $ TermRequester b

watchTermRec :: (Ord i, MonadProp m, Identifier i (TermSet i)) => i -> m i
watchTermRec ct = watch ct $ WatchTermRec ct

newtype WatchTermRec i = WatchTermRec i deriving (Eq, Ord, Show, Generic, Hashable)

instance (Identifier i (TermSet i), MonadProp m) => Propagator m  (TermSet i) (WatchTermRec i) where
    --WARNING: Does not remove listeners after join!
    propagate (WatchTermRec this) Bot = void $ watchTerm this
    propagate (WatchTermRec this) ts = do
      watchTerm this
      forM_ (variables ts) $ \v -> do
        watchTermRec v
      forM_ (applications ts) $ \(a,b) -> do
        watchTermRec a
        watchTermRec b

class CopyTermId i where
  --copy listId origTerm
  copy :: forall w. (Std w) => w -> i -> i
  --copyTermIdContents :: forall w. (Std w) => i -> Maybe (w,i)

refreshVarsTbl :: (MonadProp m, Identifier i (TermSet i), CopyTermId i, Std w)
               => w -> HashMap TermConst i -> i -> m i
refreshVarsTbl listId tbl orig = do
    watch orig $ RefreshVar listId orig tbl
    return (copy listId orig)

data RefreshVar i = forall w. Std w => RefreshVar w i (HashMap TermConst i)
instance Hashable i => Hashable (RefreshVar i) where
    hashWithSalt n (RefreshVar w i m) = hashWithSalt n (w, i, m)
instance Eq i => Eq (RefreshVar i) where
    (RefreshVar w i mp) == (RefreshVar w' i' mp') = w =~= w' && i == i' && mp == mp'
instance Ord i => Ord (RefreshVar i) where
    compare (RefreshVar w i mp) (RefreshVar w' i' mp') = compareTyped w w' <>  compare i i' <> compare mp mp'
instance Show i => Show (RefreshVar i) where
    show (RefreshVar w i mp) = "RefreshVar "++show w++" "++show i++" "++show mp


instance (MonadProp m, Identifier i (TermSet i), CopyTermId i)
         => Propagator m (TermSet i) (RefreshVar i) where
    propagate (RefreshVar listId orig _) Bot = void $ write (copy listId orig) bot
    propagate (RefreshVar listId orig tbl) ts = do
        let copyListId = copy listId orig
        watchTerm copyListId

        constant ts `forM_` (write copyListId . \c -> maybe (constTerm c) varTerm . HashMap.lookup c $ tbl)
                --important! The tc cannot be wrapped in a copy because it is not a copy!
        --There is a problem here: When an application contains variables, that have been created by this copy (e.g. by equalling the copy to its original). This creates an infinite amount of copies. Two ways to solve: Either target addresses are made relative (with applLeft and applRight), but that could cause exponential blowup between terms. Other way is to have a look into the variable, whether it was created by this copy. Problem here: If several copies are stacked on top of each other, it does not solve the problem.
        applications ts `forM_` \(a,b) -> do
            write copyListId (aplTerm (copy listId a, copy listId b))
            refreshVarsTbl listId tbl a
            refreshVarsTbl listId tbl b
