{-# LANGUAGE UndecidableInstances #-}
module Data.Terms.Terms where

import "base" Data.Maybe
import "base" Data.Function
import "base" Control.Applicative
import "base" Control.Monad

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


data TermSet a
    = TSBot
    | TS
        { constant :: Maybe TermConst
        , variables :: Set a
        , applications :: Set (a, a)
        }
  deriving (Eq, Ord, Show)


constTerms :: Ord a => TermConst -> TermSet a
constTerms c = top {constant = Just c}

appTerms :: Ord a => (a, a) -> TermSet a
appTerms app = top {applications = Set.singleton app}

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
    let propBotThis a = watch a ("propBot" :: String, a) $ propBot this
    mapM_ propBotThis . flip foldMap appList $ \(a, b) -> [a, b] :: Set i
