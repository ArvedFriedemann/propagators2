module Data.Terms.Terms where

import "base" GHC.Exts
import "base" Control.Monad

import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Some
import "this" Data.Util
import "this" Control.Combinator

import "this" Control.Combinator.Logics

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

data TermConst
    = TOP
    | BOT
    | AND
    | OR
    | IMPL
    | CUST String
    | GEN (Some Std)
    | ID Int
  deriving (Show, Eq, Ord)

instance IsString TermConst where
    fromString = CUST

data TermSet a
    = TS
        { constants :: Set TermConst
        , variables :: Set a
        , applications :: Set (a, a)
        , botOverride :: Bool
        }
  deriving (Eq, Ord, Show)

newtype TermSetPtr v = TSP (v (TermSet (TermSetPtr v)))
deriving instance (forall a. Show (v a)) => Show (TermSetPtr v)
deriving instance (forall a. Eq (v a)) => Eq (TermSetPtr v)
deriving instance (forall a. Ord (v a)) => Ord (TermSetPtr v)

unpkTSP :: TermSetPtr v -> v (TermSet (TermSetPtr v))
unpkTSP (TSP p) = p

instance HasTop (TermSet a) where
  isTop (TS a b c bo) = Set.null a && Set.null b && Set.null c && (not bo)
  top = TS Set.empty Set.empty Set.empty False

instance HasBot (TermSet a) where
  isBot ts = botOverride ts ||
              (greaterOne $ constants ts) ||
              (isSingleton $ constants ts) && (not . null . applications $ ts)
  bot = TS Set.empty Set.empty Set.empty True

instance (Ord a) => Meet (TermSet a) where
  (TS a b c bo) /\ (TS a' b' c' bo') = TS
                                        (Set.union a a')
                                        (Set.union b b')
                                        (Set.union c c')
                                        (bo || bo')

instance (Ord a) => Join (TermSet a) where
  (TS a b c bo) \/ (TS a' b' c' bo') = TS
                                        (Set.intersection a a')
                                        (Set.intersection b b')
                                        (Set.intersection c c')
                                        (bo && bo')

constTerm :: TermConst -> TermSet a
constTerm c = top {constants = Set.singleton c}

varTerm :: a -> TermSet a
varTerm v = top {variables = Set.singleton v}

aplTerm :: (a, a) -> TermSet a
aplTerm app = top {applications = Set.singleton app}

-------------------------------------------
--TermListener and Unification
-------------------------------------------

data TermListener = TermListener
  deriving (Show, Eq, Ord)

watchTerm :: (MonadProp m v scope, StdPtr v) => TermSetPtr v -> m ()
watchTerm (TSP ptr) = watch' ptr TermListener (termListener (TSP ptr))

termListener :: (MonadProp m v scope, StdPtr v) => TermSetPtr v -> TermSet (TermSetPtr v) -> m ()
termListener this@(TSP this') (TS _ variables applications _) = do
  eqAll (Set.map unpkTSP $ setAppend this variables)
  eqAll (Set.map unpkTSP $ Set.map fst applications)
  eqAll (Set.map unpkTSP $ Set.map snd applications)
  forM_ applications $ \((TSP a),(TSP b)) -> do
    propBot a this'
    propBot b this'
    propBot this' a
    propBot this' b
    --bots need to be propagated both ways, otherwise prefixes of unsuccessful matches can still be read without failure


-------------------------------------------
--Promotion
-------------------------------------------

data TermPromoter = TermPromoter
  deriving (Show, Eq, Ord)

promoteTerm :: (MonadProp m v scope, StdPtr v) => TermSetPtr v -> m ()
promoteTerm (TSP p) = watch' p TermPromoter (termPromoter (TSP p))

termPromoter :: (MonadProp m v scope, StdPtr v) => TermSetPtr v -> TermSet (TermSetPtr v) -> m ()
termPromoter this@(TSP this') (TS _ _ applications _) = do
  promote this'
  forM_ applications $ \(a,b) -> do
    promoteTerm a
    promoteTerm b

instance (MonadProp m v scope, StdPtr v) => Promoter (v (TermSet (TermSetPtr v))) m where
  promoteAction p = promoteTerm (TSP p)

-------------------------------------------
--Variable Refreshing
-------------------------------------------

data RefreshVar i a = RefreshVar i
  deriving (Show, Eq, Ord)
instance Identifier (RefreshVar i a) a

data RefreshVarsTbl i = RefreshVarsTbl i
  deriving (Show, Eq, Ord)

--TODO: return relative refresh pointer
refreshVarsTbl :: forall m v scope n. (MonadProp m v scope, Std n, StdPtr v) => n -> Map TermConst (TermSetPtr v) -> TermSetPtr v -> m (TermSetPtr v)
refreshVarsTbl ctx mp (TSP ptr) = do
  ref <- newRelative ptr (RefreshVar @_ @(TermSet (TermSetPtr v)) ctx)
  watchTerm (TSP ref)
  watch' ptr (RefreshVarsTbl ctx) (refresher ctx mp (TSP ptr) (TSP ref))
  return (TSP ref)


refresher :: (MonadProp m v scope, Std n, StdPtr v) => n -> Map TermConst (TermSetPtr v) -> TermSetPtr v -> TermSetPtr v -> TermSet (TermSetPtr v) -> m ()
refresher ctx mp this@(TSP this') newRef@(TSP ref) (TS constants _ applications _) = do
  forM_ constants $ \c -> do
    case Map.lookup c mp of
      Just (TSP p) -> write ref (varTerm (TSP p))
      Nothing -> write ref (constTerm c)
  forM_ applications $ \(a,b) -> do
    left  <- refreshVarsTbl ctx mp a
    right <- refreshVarsTbl ctx mp b
    write ref (aplTerm (left, right))
---
