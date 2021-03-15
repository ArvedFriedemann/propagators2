{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.References where

import "base" Prelude hiding ( read )
import "base" Control.Applicative
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
    SEBId :: (Identifier i a, Std i) => i -> SEBId
instance Eq SEBId where
    SEBId i == SEBId j = i =~= j
instance Ord SEBId where
    SEBId i `compare` SEBId j = compareTyped i j

type SEBIdMap = Map SEBId (Some Something)
--TODO: scopes should be some list structure. In a cell, you can go up only one scope. When reading from a distant scope, you have to go down the stack until the orig and then successively punp the value up.
data TreeCell m' v a = TreeCell {
  origScope :: v Scope
, parent :: Maybe (CellPtr m' v a) --TODO: cannot be created, needs to be existing reference
, value :: v a
, relnames :: SEBIdMap
}

data CellPtr m' v a = CP (v (TreeCell m' v a))

unpkCP :: CellPtr m' v a -> v (TreeCell m' v a)
unpkCP (CP ptr) = ptr

readCP :: (MonadRead m v) => CellPtr m' v a -> m (TreeCell m' v a)
readCP ptr = MV.read $ unpkCP ptr

readSelector :: (MonadRead m v) => (TreeCell m' v a -> b) -> CellPtr m' v a -> m b
readSelector sel ptr = sel <$> readCP ptr

createValCell :: (MonadNew m v) => a -> m (TreeCell m' v a)
createValCell v = do
  val <- MV.new v
  return $ TreeCell{parent = Nothing, value = val, relnames = Map.empty}

createTopCell :: (MonadNew m v, Value a) => m (TreeCell m' v a)
createTopCell = createValCell top

createTopCellPtr :: forall m' m v a. (MonadNew m v, Value a) => m (CellPtr m' v a)
createTopCellPtr = CP <$> (createTopCell >>= MV.new)

createValCellPtr :: (MonadNew m v) => a -> m (CellPtr m' v a)
createValCellPtr val = CP <$> (createValCell val >>= MV.new)

accessLazyNameMap :: (Std i, Identifier i b, Typeable b, MonadAtomic v m' m) => m SEBIdMap -> m' SEBIdMap -> (i -> b -> m' ()) -> m' b -> i -> m b
accessLazyNameMap getMap getMap' putMap con adr = do
  mabVal <- Map.lookup (SEBId adr) <$> getMap
  case mabVal of
    Just val -> return $ fromJust $ fromSome val
    Nothing -> atomically $ do
      mabVal2 <- Map.lookup (SEBId adr) <$> getMap'
      case mabVal2 of
        Just val -> return $ fromJust $ fromSome val
        Nothing -> do
          nptr <- con
          putMap adr nptr
          return nptr

accessRelName :: forall m' m v a i b. (Std i, Identifier i b, Typeable b, MonadAtomic v m' m) => CellPtr m' v a -> m' b -> i -> m b
accessRelName ptr con adr =
  accessLazyNameMap
    (readSelector relnames ptr)
    (readSelector relnames ptr)
    (\adr' nptr -> void $ MV.mutate (unpkCP ptr) (\cp -> (cp{relnames = Map.insert (SEBId adr') (Some nptr) (relnames cp)},nptr)))
    con
    adr

writeLattPtr :: (MonadMutate m v, Value a) => v a -> a -> m Bool
writeLattPtr ptr val = MV.mutate ptr (\old -> meetDiff val old)

data PropArgs v = PropArgs {
  scopePath :: [v Scope]
, createdPointers :: v SEBIdMap
}


instance (Dep m v
        , MonadFork m
        , MonadReader (PropArgs v) m
        , MonadVar m v
        , MonadVar m' v
        , MonadAtomic v m' m
        , forall a. Std (v a)
        , Typeable m
        , Typeable m'
        , Typeable v) => MonadProp m (CellPtr m' v) where

  read :: CellPtr m' v a -> m a
  read ptr = getScopeRef ptr >>= readCP >>= MV.read . value


  write :: (Value a) => CellPtr m' v a -> a -> m ()
  write ptr val = do
    cp <- readCP ptr
    hasChanged <- writeLattPtr (value cp) val
    if hasChanged
    then notify ptr
    else return ()

  new :: (Identifier n a, Value a, Std n) => n -> m (CellPtr m' v a)
  new name = undefined{- do
    addr <- gets addresses
    pt <- topTreeCellPtr
    --tries to add the new pointer into the map. If the value at the name already exists, the old one is kept and returned as Just ...
    maybeOld <- MV.mutate addr (flip . Map.insertLookupWithKey (const . const) name pt)
    case maybeOld of
      Just old -> return ()--delete pointer
      Nothing -> return () --TODO: put parent and propagators?
      -}

data ScopedPtr m' v a = ScopedPtr (v Scope)
  deriving (Typeable)
deriving instance (forall a. Show (v a)) => Show (ScopedPtr m' v a)
deriving instance (forall a. Eq (v a)) => Eq (ScopedPtr m' v a)
deriving instance (forall a. Ord (v a)) => Ord (ScopedPtr m' v a)

instance (Typeable a, forall a. Show (v a), forall a. Eq (v a), forall a. Ord (v a), forall a. Typeable (v a), Typeable m', Typeable v) => Std (ScopedPtr (m' :: * -> *) (v :: * -> *) a)
instance Identifier (ScopedPtr m' v a) (CellPtr m' v a)

getScopeRef :: forall (m' :: * -> *) (m :: * -> *) v a. (Monad m, MonadRead m v, Eq (v Scope), Std (v Scope)) => CellPtr m' v a -> m (CellPtr m' v a)
getScopeRef ptr = do
  tc <- readCP ptr
  s <- scope
  if origScope tc == s
  then return ptr
  else do
    ptr' <- unsafeParScoped $ getScopeRef ptr
    accessRelName @m' @m ptr' (createTopCellPtr @m' @m') (ScopedPtr @m' s)

unsafeParScoped :: m a -> m a
unsafeParScoped = undefined

scope :: m (v Scope)
scope = undefined

data PropOf m' m v = PropOf
  deriving (Show, Eq, Ord, Typeable)
instance (Typeable m', Typeable m, Typeable v) => Std (PropOf (m' :: * -> *) (m :: * -> *) (v :: * -> *))

instance Identifier (PropOf m' m v) (CellPtr m' v (Map (Some Std) (m ())) )

notify :: forall (m' :: * -> *) m v a.
  ( MonadAtomic v m' m
  , MonadFork m
  , MonadProp m (CellPtr m' v)
  , Typeable m', Typeable m, Typeable v) => CellPtr m' v a -> m ()
notify ptr = do
  propset <- read =<< accessRelName ptr (createValCellPtr @m' Map.empty) (PropOf @m' @m @v)
  forkF (Map.elems propset)


--
