{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.References where

import "base" Prelude hiding ( read )
import "base" Control.Applicative
import "base" Control.Monad
import "base" Data.Maybe
import "base" Data.Typeable

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

import "mtl" Control.Monad.State.Class


data SEBId where
    SEBId :: (Identifier i a, Std i) => i -> SEBId
instance Eq SEBId where
    SEBId i == SEBId j = i =~= j
instance Ord SEBId where
    SEBId i `compare` SEBId j = compareTyped i j

data TreeCell v a = TreeCell {
  parent :: Maybe (CellPtr v a) --TODO: cannot be created, needs to be existing reference
, value :: v a
, relnames :: Map SEBId (Some Something)
}

data CellPtr v a = CP (v (TreeCell v a))

unpkCP :: CellPtr v a -> v (TreeCell v a)
unpkCP (CP ptr) = ptr

readCP :: (MonadRead m v) => CellPtr v a -> m (TreeCell v a)
readCP ptr = MV.read $ unpkCP ptr

readSelector :: (MonadRead m v) => (TreeCell v a -> b) -> CellPtr v a -> m b
readSelector sel ptr = sel <$> readCP ptr

createValCell :: (MonadNew m v) => a -> m (TreeCell v a)
createValCell v = do
  val <- MV.new v
  return $ TreeCell{parent = Nothing, value = val, relnames = Map.empty}

createTopCell :: (MonadNew m v, Value a) => m (TreeCell v a)
createTopCell = createValCell top

createTopCellPtr :: (MonadNew m v, Value a) => m (CellPtr v a)
createTopCellPtr = CP <$> (createTopCell >>= MV.new)

createValCellPtr :: (MonadNew m v) => a -> m (CellPtr v a)
createValCellPtr val = CP <$> (createValCell val >>= MV.new)

accessLazyNameMap :: (Std i, Identifier i b, Typeable b, MonadAtomic v m' m) => m (Map SEBId (Some Something)) -> m' (Map SEBId (Some Something)) -> (i -> b -> m' ()) -> m' b -> i -> m b
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

accessRelName :: (Std i, Identifier i b, Typeable b, MonadAtomic v m' m) => CellPtr v a -> m' b -> i -> m b
accessRelName ptr con adr =
  accessLazyNameMap
    (readSelector relnames ptr)
    (readSelector relnames ptr)
    (\adr' nptr -> void $ MV.mutate (unpkCP ptr) (\cp -> (cp{relnames = Map.insert (SEBId adr') (Some nptr) (relnames cp)},nptr)))
    con
    adr

writeLattPtr :: (MonadMutate m v, Value a) => v a -> a -> m Bool
writeLattPtr ptr val = MV.mutate ptr (\old -> meetDiff val old)

data PropState v = PropState {
  addresses :: forall a b. (Ord a) =>  v (Map a (v b))
}


instance (Dep m v
        , MonadFork m
        , MonadState (PropState v) m
        , MonadVar m v
        , MonadVar m' v
        , MonadAtomic v m' m
        , Typeable m
        , Typeable v) => MonadProp m' m (CellPtr v) where

  read :: CellPtr v a -> m a
  read ptr = readCP ptr >>= MV.read . value

  write :: (Value a) => CellPtr v a -> a -> m ()
  write ptr val = do
    cp <- readCP ptr
    hasChanged <- writeLattPtr (value cp) val
    if hasChanged
    then notify ptr
    else return ()

  new :: (Identifier n a, Value a, Std n) => n -> m (CellPtr v a)
  new name = undefined{- do
    addr <- gets addresses
    pt <- topTreeCellPtr
    --tries to add the new pointer into the map. If the value at the name already exists, the old one is kept and returned as Just ...
    maybeOld <- MV.mutate addr (flip . Map.insertLookupWithKey (const . const) name pt)
    case maybeOld of
      Just old -> return ()--delete pointer
      Nothing -> return () --TODO: put parent and propagators?
      -}

data PropOf m v = PropOf
  deriving (Show, Eq, Ord, Typeable)
instance (Typeable k, Typeable l, Typeable m, Typeable v) => Std (PropOf (m :: k -> *) (v :: l -> *))

instance Identifier (PropOf m v) (CellPtr v (Map (Some Std) (m ())) )

notify :: forall (m' :: * -> *) m v a.
  ( MonadAtomic v m' m
  , MonadFork m
  , MonadProp m' m (CellPtr v)
  , Typeable m, Typeable v) => CellPtr v a -> m ()
notify ptr = do
  propset <- read =<< accessRelName ptr (createValCellPtr @m' Map.empty) (PropOf @m @v)
  forkF (Map.elems propset)


--
