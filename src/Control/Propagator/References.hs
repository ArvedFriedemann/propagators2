{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.References where

import "base" Prelude hiding ( read )
import "base" Control.Applicative hiding (some)
import "base" Control.Monad
import "base" Data.Maybe
import "base" Data.Typeable
import "base" Debug.Trace

import "mtl" Control.Monad.Reader

import "this" Control.MonadVar.MonadVar (MonadNew, MonadMutate, MonadRead)
import qualified "this" Control.MonadVar.MonadVar as MV

import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Some
import "this" Data.Typed

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set



data SEBId where
  --TODO WARNING: Warning, typesafety is now only from the outside!
    SEBId :: {-(Identifier i a,-}( Std i) => i -> SEBId
instance Eq SEBId where
    SEBId i == SEBId j = i =~= j
instance Ord SEBId where
    SEBId i `compare` SEBId j = compareTyped i j

type SEBIdMap = Map SEBId (Some Something)
--TODO: scopes should be some list structure. In a cell, you can go up only one scope. When reading from a distant scope, you have to go down the stack until the orig and then successively punp the value up.
data TreeCell m' (v :: * -> *) (a :: *) = TreeCell {
  origScope :: Scope v
, parent :: Maybe (CellPtr m' v a) --TODO: cannot be created, needs to be existing reference
, value :: v a
, relnames :: v SEBIdMap
, scopenames :: v (Map (Scope v) (CellPtr m' v a))
}

data CellPtr m' (v :: * -> *) (a :: *) = CP (v (TreeCell m' v a))
deriving instance (forall b. Show (v b)) => Show (CellPtr m' v a)
deriving instance (forall b. Eq (v b)) => Eq (CellPtr m' v a)
deriving instance (forall b. Ord (v b)) => Ord (CellPtr m' v a)

unpkCP :: CellPtr m' v a -> v (TreeCell m' v a)
unpkCP (CP ptr) = ptr

readCP :: (MonadRead m v) => CellPtr m' v a -> m (TreeCell m' v a)
readCP ptr = MV.read $ unpkCP ptr

readSelector :: forall m' m v a b. (MonadRead m v) => (TreeCell m' v a -> b) -> CellPtr m' v a -> m b
readSelector sel ptr = sel <$> readCP ptr

readValue :: (MonadRead m v) => CellPtr m' v a -> m a
readValue ptr = readCP ptr >>= MV.read . value

createValCell :: (MonadNew m v) => Scope v -> a -> m (TreeCell m' v a)
createValCell s v = do
  val <- MV.new v
  reln <- MV.new Map.empty
  snms <- MV.new Map.empty
  return $ TreeCell{parent = Nothing, origScope = s, value = val, relnames = reln, scopenames = snms}

createTopCell :: (MonadNew m v, Value a) => Scope v -> m (TreeCell m' v a)
createTopCell s = createValCell s top

createTopCellPtr :: forall m' m v a. (MonadNew m v, Value a) => Scope v -> m (CellPtr m' v a)
createTopCellPtr s = CP <$> (createTopCell s >>= MV.new)

createValCellPtr :: (MonadNew m v) => Scope v -> a -> m (CellPtr m' v a)
createValCellPtr s val = CP <$> (createValCell s val >>= MV.new)

--TODO: When using a shared namespace for the scopes, there should be a mechanic where one can see the old value here. If a variable is created on a lower scope, it should be put in this place and be pumped up.
accessLazyNameMap :: (Ord i, MonadAtomic v m' m) => m (Map i b) -> m' (Map i b) -> (i -> b -> m' ()) -> m' b -> (b -> m ()) -> i -> m b
accessLazyNameMap getMap getMap' putMap con destr adr = do
  mabVal <- Map.lookup adr <$> getMap
  case mabVal of
    Just val -> return val
    Nothing -> do
      (res, hasChanged) <- atomically $ do
        mabVal2 <- Map.lookup adr <$> getMap'
        case mabVal2 of
          Just val -> return (val, False)
          Nothing -> do
            nptr <- con
            putMap adr nptr
            return (nptr, True)
      when hasChanged (destr res)
      return res

accessLazyNameMap' :: forall m' m v i b. (Std i, Typeable b, MonadAtomic v m' m) => v SEBIdMap -> m' b -> i -> m b
accessLazyNameMap' ptr con adr =
  fromJust . fromSome <$> accessLazyNameMap
    (MV.read ptr)
    (MV.read ptr)
    (\adr' nptr -> void $ MV.mutate ptr (\mp -> (Map.insert adr' nptr mp,nptr)))
    (Some <$> con)
    (const $ return ())
    (SEBId adr)

accessLazyTypeMap' :: forall m' m v a b. (Ord a, MonadAtomic v m' m) => v (Map a b) -> m' b -> a -> m b
accessLazyTypeMap' ptr con adr =
  accessLazyNameMap
    (MV.read ptr)
    (MV.read ptr)
    (\adr' nptr -> void $ MV.mutate ptr (\mp -> (Map.insert adr' nptr mp,nptr)))
    con
    (const $ return ())
    adr

accessRelName :: forall m' m v a i b. (Std i, Identifier i b, Typeable b, MonadAtomic v m' m) => CellPtr m' v a -> m' b -> i -> m b
accessRelName ptr con adr = do
  mp <- readSelector relnames ptr
  fromJust . fromSome <$> accessLazyNameMap
    (MV.read mp)
    (MV.read mp)
    (\adr' nptr -> void $ MV.mutate mp (\mp' -> (Map.insert adr' nptr mp',nptr)))
    (Some <$> con)
    (const $ return ())
    (SEBId adr)

data ScopeEq = ScopeEq
  deriving (Show, Eq, Ord)

accessScopeName :: forall m' m (v :: * -> *) a.
  ( Value a, Typeable v, Ord (Scope v)
  , MonadAtomic v m' m
  , MonadReader (PropArgs m m' v) m
  , MonadFork m
  , Typeable m, Typeable m', forall b. Show (v b), forall b. Ord (v b) ) => Scope v -> CellPtr m' v a -> Scope v -> m (CellPtr m' v a)
accessScopeName currscp ptr scp = do
  mp <- readSelector scopenames ptr
  accessLazyNameMap
    (MV.read mp)
    (MV.read mp)
    (\adr' nptr -> void $ MV.mutate mp (\mp' -> (Map.insert adr' nptr mp',nptr)))
    (createTopCellPtr currscp)
    (\nptr -> watchEngine ptr ScopeEq (readEngine ptr >>= \b -> (traceM $ "promoting value " ++show b++" in "++show ptr++" into "++show nptr) >> writeEngine nptr b))
    scp

writeLattPtr :: (MonadMutate m v, Value a) => v a -> a -> m Bool
writeLattPtr ptr val = MV.mutate ptr (\old -> meetDiff val old)

--TODO: Just make new namespaces for each scope. It's the simplest solution for now.
data PropArgs m m' v = PropArgs {
  scopePath :: [Scope v]
, createdScopes :: v SEBIdMap
, fixpointActions :: v (Map (Some Std) (PropArgs m m' v, m ()))
, fixpointSemaphore :: v Int
}

data ScopeT v = ScopeT {
  createdPointers :: v SEBIdMap
}

newtype Scope (v :: * -> *) = SP (v (ScopeT v))
deriving instance (forall a. Show (v a)) => Show (Scope v)
deriving instance (forall a. Eq (v a)) => Eq (Scope v)
deriving instance (forall a. Ord (v a)) => Ord (Scope v)

unpkSP :: Scope v -> v (ScopeT v)
unpkSP (SP x) = x

instance (Dep m v
        , MonadFork m
        , MonadReader (PropArgs m m' v) m
        , MonadVar m v
        , MonadVar m' v
        , MonadAtomic v m' m
        --, forall a. (Typeable a) => Std (v a)
        , forall a. Eq (v a)
        , forall a. Ord (v a)
        , forall a. Show (v a)
        , Typeable m
        , Typeable m'
        , Typeable v) => MonadProp (m :: * -> *) (CellPtr (m' :: * -> *) v) (Scope v) where

  read :: (Value a) => CellPtr m' v a -> m a
  read ptr = readEngine ptr

  write :: (Value a) => CellPtr m' v a -> a -> m ()
  write ptr val = writeEngine ptr val

  watch :: (Value a, Std n) => CellPtr m' v a -> n -> m () -> m ()
  watch ptr name act = watchEngine ptr name act

  new :: (Identifier n a, Value a, Std n) => n -> m (CellPtr m' v a)
  new name = do
    s <- scope
    sp <- MV.read (unpkSP s)
    accessLazyNameMap' (createdPointers sp) (createTopCellPtr @m' s) name

  newRelative :: (Identifier n a, Value a, Std n) => CellPtr m' v b -> n -> m (CellPtr m' v a)
  newRelative ptr name = do
    s <- scope
    rn <- readSelector relnames ptr
    accessLazyNameMap' rn (createTopCellPtr @m' s) name

  currScopePtr :: (Value a) => CellPtr m' v a -> m (CellPtr m' v a)
  currScopePtr ptr = getScopeRef ptr

  newScope :: (Std n) => n -> m (Scope v)
  newScope name = do
    mp <- reader createdScopes
    accessLazyNameMap' mp (do
      pts <- MV.new Map.empty
      sp <- MV.new $ ScopeT{createdPointers = pts}
      return $ SP sp) name

  --TODO: WARNING! Currently, name map is always the same
  scoped :: (Scope v) -> m () -> m ()
  scoped sp = local (\p -> p{scopePath = sp : (scopePath p)})

  parScoped :: m () -> m ()
  parScoped = local (\p -> p{scopePath = tail (scopePath p)})

  watchFixpoint :: (Std n) => n -> m () -> m ()
  watchFixpoint name act = do
    state <- ask
    fixP <- reader fixpointActions
    void $ accessLazyTypeMap' fixP (return (state, act)) (Some name)



readEngine :: forall (m' :: * -> *) m v a.
  ( MonadAtomic v m' m
  , MonadReader (PropArgs m m' v) m
  , MonadFork m
  , Typeable m, Typeable m', Typeable v, forall b. Show (v b), forall b. Ord (v b), Value a) => CellPtr m' v a -> m a
readEngine ptr = getScopeRef ptr >>= readCP >>= MV.read . value

writeEngine :: forall (m' :: * -> *) m v a. (Monad m, MonadAtomic v m' m, MonadReader (PropArgs m m' v) m, Typeable v, forall b. Show (v b), forall b. Ord (v b), MonadFork m, Typeable m', Typeable m, Value a) => CellPtr m' v a -> a -> m ()
writeEngine ptr val = do
  traceM $ "writing "++show val++" into " ++ show ptr
  ptr' <- getScopeRef ptr
  cp <- readCP ptr'
  old <- MV.read (value cp)
  hasChanged <- writeLattPtr (value cp) val
  if hasChanged
  then notify ptr'
  else return ()

watchEngine :: forall (m' :: * -> *) m v a n. (Typeable m', Typeable m, Typeable v, MonadAtomic v m' m, MonadReader (PropArgs m m' v) m, forall b. Show (v b), Value a, Std n) => CellPtr m' v a -> n -> m () -> m ()
watchEngine ptr name act = do
  traceM $ "watching "++show ptr++" with " ++ show name
  propset <- getPropset @m' ptr >>= readSelector @m' value
  hasChanged <- MV.mutate propset (\ps -> let (mabChan,mp) = Map.insertLookupWithKey (\k n o -> n) (Some name) act ps in (mp, not $ isJust mabChan))
  when hasChanged act

--TODO: WARNING: this only works, when the reference comes from below! when references come from aboce, they'd need to be pumped up!
getScopeRef :: forall (m' :: * -> *) (m :: * -> *) v a.
  ( Monad m
  , MonadRead m v
  , MonadScope m v
  , MonadAtomic v m' m
  , MonadReader (PropArgs m m' v) m
  , MonadFork m
  , MonadUnsafeParScoped m
  , Typeable m, Typeable m', forall b. Eq (v b), forall b. Ord (v b), forall b. Show (v b), Typeable v, Value a) => CellPtr m' v a -> m (CellPtr m' v a)
getScopeRef ptr = do
  tc <- readCP ptr
  s <- scope
  if origScope tc == s
  then return ptr
  else do
    ptr' <- unsafeParScoped $ getScopeRef ptr
    accessScopeName s ptr' s

class MonadUnsafeParScoped m where
  --Unsafe because this parScoped can return values and therefore potentially more pointers from forks to the orig, which should be avoided at this stage
  unsafeParScoped :: m a -> m a

instance (MonadReader (PropArgs m m' v) m) => MonadUnsafeParScoped m where
  unsafeParScoped :: m a -> m a
  unsafeParScoped = local (\p -> p{scopePath = tail (scopePath p)})

class MonadScope m v where
  scope ::m (Scope v)

instance (MonadReader (PropArgs m m' v) m) => MonadScope m v where
  scope :: m (Scope v)
  scope = head <$> reader scopePath

data PropOf m' m v = PropOf
  deriving (Show, Eq, Ord, Typeable)

type PropSetPtr m' m v = CellPtr m' v (Map (Some Std) (m ()))
instance Identifier (PropOf m' m v) (PropSetPtr m' m v)

notify :: forall (m' :: * -> *) m v a.
  ( MonadAtomic v m' m
  , MonadFork m
  , MonadScope m v
  , forall b. Show (v b)
  , Typeable m', Typeable m, Typeable v) => CellPtr m' v a -> m ()
notify ptr = do
  propset <- getPropset ptr >>= readValue
  forkF (Map.elems propset)

getPropset :: forall (m' :: * -> *) m v a. (Typeable m', Typeable m, Typeable v, MonadAtomic v m' m, MonadScope m v, forall b. Show (v b)) => CellPtr m' v a -> m (PropSetPtr m' m v)
getPropset ptr = do
  s <- scope
  accessRelName ptr (createValCellPtr @m' s Map.empty) (PropOf @m' @m @v)

--
