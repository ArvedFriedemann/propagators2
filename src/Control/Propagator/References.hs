{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.References where

import "base" Prelude hiding ( read )
import "base" Control.Applicative hiding (some)
import "base" Control.Monad
import "base" Data.Maybe
import "base" Data.Typeable

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
, relnames :: SEBIdMap
, scopenames :: Map (Scope v) (CellPtr m' v a)
}

data CellPtr m' (v :: * -> *) (a :: *) = CP (v (TreeCell m' v a))
deriving instance (forall b. Show (v b)) => Show (CellPtr m' v a)
deriving instance (forall b. Eq (v b)) => Eq (CellPtr m' v a)
deriving instance (forall b. Ord (v b)) => Ord (CellPtr m' v a)

unpkCP :: CellPtr m' v a -> v (TreeCell m' v a)
unpkCP (CP ptr) = ptr

readCP :: (MonadRead m v) => CellPtr m' v a -> m (TreeCell m' v a)
readCP ptr = MV.read $ unpkCP ptr

readSelector :: (MonadRead m v) => (TreeCell m' v a -> b) -> CellPtr m' v a -> m b
readSelector sel ptr = sel <$> readCP ptr

readValue :: (MonadRead m v) => CellPtr m' v a -> m a
readValue ptr = readCP ptr >>= MV.read . value

createValCell :: (MonadNew m v) => a -> m (TreeCell m' v a)
createValCell v = do
  val <- MV.new v
  s <- scope
  return $ TreeCell{parent = Nothing, origScope = s, value = val, relnames = Map.empty, scopenames = Map.empty}

createTopCell :: (MonadNew m v, Value a) => m (TreeCell m' v a)
createTopCell = createValCell top

createTopCellPtr :: forall m' m v a. (MonadNew m v, Value a) => m (CellPtr m' v a)
createTopCellPtr = CP <$> (createTopCell >>= MV.new)

createValCellPtr :: (MonadNew m v) => a -> m (CellPtr m' v a)
createValCellPtr val = CP <$> (createValCell val >>= MV.new)

--TODO: Hwn using a shared namespace for the scopes, there should be a mechanic where one can see the old value here. If a variable is created on a lower scope, it should be put in this place and be pumped up.
accessLazyNameMap :: (Ord i, MonadAtomic v m' m) => m (Map i b) -> m' (Map i b) -> (i -> b -> m' ()) -> m' b -> i -> m b
accessLazyNameMap getMap getMap' putMap con adr = do
  mabVal <- Map.lookup adr <$> getMap
  case mabVal of
    Just val -> return val
    Nothing -> atomically $ do
      mabVal2 <- Map.lookup adr <$> getMap'
      case mabVal2 of
        Just val -> return val
        Nothing -> do
          nptr <- con
          putMap adr nptr
          return nptr

accessLazyNameMap' :: forall m' m v i b. (Std i, Typeable b, MonadAtomic v m' m) => v SEBIdMap -> m' b -> i -> m b
accessLazyNameMap' ptr con adr =
  fromJust . fromSome <$> accessLazyNameMap
    (MV.read ptr)
    (MV.read ptr)
    (\adr' nptr -> void $ MV.mutate ptr (\mp -> (Map.insert adr' (Some nptr) mp,nptr)))
    (Some <$> con)
    (SEBId adr)

accessLazyTypeMap' :: forall m' m v a b. (Ord a, MonadAtomic v m' m) => v (Map a b) -> m' b -> a -> m b
accessLazyTypeMap' ptr con adr =
  accessLazyNameMap
    (MV.read ptr)
    (MV.read ptr)
    (\adr' nptr -> void $ MV.mutate ptr (\mp -> (Map.insert adr' nptr mp,nptr)))
    con
    adr

accessRelName :: forall m' m v a i b. (Std i, Identifier i b, Typeable b, MonadAtomic v m' m) => CellPtr m' v a -> m' b -> i -> m b
accessRelName ptr con adr =
  fromJust . fromSome <$> accessLazyNameMap
    (readSelector relnames ptr)
    (readSelector relnames ptr)
    (\adr' nptr -> void $ MV.mutate (unpkCP ptr) (\cp -> (cp{relnames = Map.insert adr' (Some nptr) (relnames cp)},nptr)))
    (Some <$> con)
    (SEBId adr)

accessScopeName :: forall m' m (v :: * -> *) a. (Value a, Typeable v, Ord (Scope v), MonadAtomic v m' m) => CellPtr m' v a -> Scope v -> m (CellPtr m' v a)
accessScopeName ptr scp =
  accessLazyNameMap
    (readSelector scopenames ptr)
    (readSelector scopenames ptr)
    (\adr' nptr -> void $ MV.mutate (unpkCP ptr) (\cp -> (cp{scopenames = Map.insert adr' nptr (scopenames cp)},nptr)))
    createTopCellPtr
    scp

writeLattPtr :: (MonadMutate m v, Value a) => v a -> a -> m Bool
writeLattPtr ptr val = MV.mutate ptr (\old -> meetDiff val old)

--TODO: Just make new namespaces for each scope. It's the simplest solution for now.
data PropArgs m m' v = PropArgs {
  scopePath :: [Scope v]
, createdScopes :: v SEBIdMap
, fixpointActions :: v (Map (Some Std) (m ()))
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
        , forall a. Std (v a)
        , Typeable m
        , Typeable m'
        , Typeable v) => MonadProp (m :: * -> *) (CellPtr (m' :: * -> *) v) (Scope v) where

  read :: (Value a) => CellPtr m' v a -> m a
  read ptr = getScopeRef ptr >>= readCP >>= MV.read . value

  write :: (Value a) => CellPtr m' v a -> a -> m ()
  write ptr val = do
    ptr' <- getScopeRef ptr
    cp <- readCP ptr'
    hasChanged <- writeLattPtr (value cp) val
    if hasChanged
    then notify ptr'
    else return ()

  watch :: (Value a, Std n) => CellPtr m' v a -> n -> m () -> m ()
  watch ptr name act = do
    propset <- getPropset ptr >>= readSelector value
    hasChanged <- MV.mutate propset (\ps -> let (mabChan,mp) = Map.insertLookupWithKey (\k n o -> n) (Some name) act ps in (mp, not $ isJust mabChan))
    when hasChanged act

  new :: (Identifier n a, Value a, Std n) => n -> m (CellPtr m' v a)
  new name = do
    s <- scope >>= MV.read . unpkSP
    accessLazyNameMap' (createdPointers s) (createTopCellPtr @m') name

  newScope :: (Identifier n (Scope v), Std n) => n -> m (Scope v)
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

  watchFixpoint :: (Identifier n (m ()), Std n) => n -> m () -> m ()
  watchFixpoint name act = do
    fixP <- reader fixpointActions
    void $ accessLazyTypeMap' fixP (return act) (Some name)



--TODO: WARNING: this only works, when the reference comes from below! when references come from aboce, they'd need to be pumped up!
getScopeRef :: forall (m' :: * -> *) (m :: * -> *) v a. (Monad m, MonadRead m v, MonadAtomic v m' m, forall b. Eq (v b), forall b. Ord (v b), Typeable v, Value a) => CellPtr m' v a -> m (CellPtr m' v a)
getScopeRef ptr = do
  tc <- readCP ptr
  s <- scope
  if origScope tc == s
  then return ptr
  else do
    ptr' <- unsafeParScoped $ getScopeRef ptr
    accessScopeName ptr' s

unsafeParScoped :: m a -> m a
unsafeParScoped = undefined

scope :: m (Scope v)
scope = undefined

data PropOf m' m v = PropOf
  deriving (Show, Eq, Ord, Typeable)

type PropSetPtr m' m v = CellPtr m' v (Map (Some Std) (m ()))
instance Identifier (PropOf m' m v) (PropSetPtr m' m v)

notify :: forall (m' :: * -> *) m v a.
  ( MonadAtomic v m' m
  , MonadFork m
  , Typeable m', Typeable m, Typeable v) => CellPtr m' v a -> m ()
notify ptr = do
  propset <- getPropset ptr >>= readValue
  forkF (Map.elems propset)

getPropset :: forall (m' :: * -> *) m v a. (Typeable m', Typeable m, Typeable v, MonadAtomic v m' m) => CellPtr m' v a -> m (PropSetPtr m' m v)
getPropset ptr = accessRelName ptr (createValCellPtr @m' Map.empty) (PropOf @m' @m @v)

--
