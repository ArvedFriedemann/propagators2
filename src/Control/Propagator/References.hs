{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.References where

import "base" Prelude hiding ( read )
import "base" Control.Applicative
import "base" Control.Monad
import "base" Data.Maybe

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
, relnames :: Map SEBId (Some Value)
}

data CellPtr v a = CP (v (TreeCell v a))

unpkCP :: CellPtr v a -> v (TreeCell v a)
unpkCP (CP ptr) = ptr

readCP :: (MonadRead m v) => CellPtr v a -> m (TreeCell v a)
readCP ptr = MV.read $ unpkCP ptr

readSelector :: (MonadRead m v) => (TreeCell v a -> b) -> CellPtr v a -> m b
readSelector sel ptr = sel <$> readCP ptr

accessLazyNameMap :: (Std i, Identifier i b, Value b, MonadAtomic v m' m) => m (Map SEBId (Some Value)) -> m' (Map SEBId (Some Value)) -> (i -> b -> m' ()) -> m' b -> i -> m b
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

accessRelName :: (Std i, Identifier i b, Value b, MonadAtomic v m' m) => CellPtr v a -> m' b -> i -> m b
accessRelName ptr con adr =
  accessLazyNameMap
    (readSelector relnames ptr)
    (readSelector relnames ptr)
    (\adr' nptr -> void $ MV.mutate (unpkCP ptr) (\cp -> (cp{relnames = Map.insert (SEBId adr') (Some nptr) (relnames cp)},nptr)))
    con
    adr



data PropState v = PropState {
  addresses :: forall a b. (Ord a) =>  v (Map a (v b))
}


instance (Dep m v
        , MonadFork m
        , MonadState (PropState v) m
        , MonadNew m v
        , MonadMutate m v
        , MonadRead m v) => MonadProp m (CellPtr v) where

  read :: (Value a) => CellPtr v a -> m a
  read ptr = readCP ptr >>= MV.read . value

  write :: (Value a) => CellPtr v a -> a -> m ()
  write ptr val = undefined {- do
    cp <- readCP ptr
    hasChanged <- writeLattPtr (value cp) val
    if hasChanged
    then notify ptr
    else return () -}

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

{-}
notify :: (MonadRead m v, MonadFork m) => CellPtr v a -> m ()
notify ptr = do
  propset <- readCPPropagators ptr >>= mapM MV.read . Set.toList
  forkF propset
-}

--
