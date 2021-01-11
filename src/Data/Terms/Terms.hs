{-# LANGUAGE UndecidableInstances #-}
module Data.Terms.Terms where

import "base" Data.Maybe
import "base" Data.Function
import "base" Control.Applicative
import "base" Control.Monad
import "base" GHC.Exts

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

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
{-# COMPLETE Bot, TS #-}

instance IsString TermConst where
    fromString = CUST
instance IsString (TermSet a) where
    fromString = constTerm . CUST

constTerm :: TermConst -> TermSet a
constTerm c = top {constant = Just c}

varTerm :: a -> TermSet a
varTerm v = top {variables = Set.singleton v}

aplTerm :: (a, a) -> TermSet a
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
instance HasTop (TermSet a) where
    isTop (TS Nothing v a) = Set.null v && Set.null a
    isTop _ = False
    top = TS Nothing Set.empty Set.empty
instance Ord a => Join (TermSet a) where
    (\/) = liftTS2 Set.intersection
instance HasBot (TermSet a) where
    isBot TSBot = True
    isBot _ = False
    bot = TSBot
instance Ord a => BoundedMeet (TermSet a)
instance Ord a => BoundedJoin (TermSet a)
instance Ord a => Lattice (TermSet a)
instance Ord a => BoundedLattice (TermSet a)

data PropBot i = PropBot i deriving (Eq, Ord, Show)
instance (MonadProp m, Value b, Identifier i b, BoundedJoin b, Value a, BoundedJoin a) => Propagator m a (PropBot i) where
    propagate (PropBot i) a = when (a == Bot) . void $ write i Bot

watchTerm :: (Ord i, MonadProp m, Identifier i (TermSet i)) => i -> m i
watchTerm ct = watch ct $ TermListener ct

data TermListener i = TermListener i deriving (Eq, Ord, Show)

instance (Identifier i (TermSet i), MonadProp m) => Propagator m  (TermSet i) (TermListener i) where
    --WARNING: Does not remove listeners after join!
    propagate _ Bot = pure ()
    propagate (TermListener this) ts = do
        --equality for variables
        eqAll . Set.insert this . variables $ ts
        --equality for applications
        let appList = Set.toList . applications $ ts
        eqAll $ fst <$> appList
        eqAll $ snd <$> appList
        --as subvalues are not equivalent to this value, their bots have to be propagated as well
        let propBotThis a = watch a $ PropBot this
        mapM_ propBotThis $ fst <$> appList
        mapM_ propBotThis $ snd <$> appList


class Std w => CopyTermId w i | i -> w where
  --copy listId origTerm
  copy :: w -> i -> i
  copyTermIdContents :: i -> Maybe (w,i)

refreshVarsTbl :: (MonadProp m, Identifier i (TermSet i), CopyTermId w i)
               => w -> Map TermConst i -> i -> m i
refreshVarsTbl listId tbl orig = do
    watch orig $ RefreshVar listId orig tbl
    return (copy listId orig)

data RefreshVar w i = RefreshVar w i (Map TermConst i)
  deriving (Eq, Ord, Show)
instance (MonadProp m, Identifier i (TermSet i), CopyTermId w i)
         => Propagator m (TermSet i) (RefreshVar w i) where
    propagate (RefreshVar listId orig _) Bot = void $ write (copy listId orig) bot
    propagate (RefreshVar listId orig tbl) ts = do
        let copyListId = copy listId orig

        watchTerm copyListId

        constant ts `forM_` (write copyListId . \c -> maybe (constTerm c) varTerm . Map.lookup c $ tbl)
                --important! The tc cannot be wrapped in a copy because it is not a copy!

        applications ts `forM_` \(a,b) -> do
            write copyListId (aplTerm (copy listId a, copy listId b))
            watch a $ RefreshVar listId a tbl
            watch b $ RefreshVar listId b tbl
