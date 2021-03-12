{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.References where

import "base" Prelude hiding ( read )
import "base" Control.Applicative
import "base" Data.Maybe

import "this" Control.MonadVar.MonadVar (MonadNew, MonadMutate, MonadRead)
import qualified "this" Control.MonadVar.MonadVar as MV
import "this" Control.Propagator.Class
import "this" Data.Lattice

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

import "mtl" Control.Monad.State.Class


data SEBId where
    SEBId :: Identifier i a => i -> SEBId
instance Eq SEBId where
    SEBId s i == SEBId t j = s == t && i =~= j
instance Ord SEBId where
    SEBId s i `compare` SEBId t j = compare s t <> compareTyped i j

data TreeCell m v a = TreeCell {
  parent :: Maybe (CellPtr m v a) --TODO: cannot be created, needs to be existing reference
, value :: v a
, relnames :: Map SEBId (Some Value)
}

data CellPtr m v a = CP (v (TreeCell m v a))

unpkCP :: CellPtr m v a -> v (TreeCell m v a)
unpkCP (CP ptr) = ptr

readCP :: (MonadRead m v) => CellPtr m v a -> m (TreeCell m v a)
readCP ptr = MV.read $ unpkCP ptr

readSelector :: (MonadRead m v) => (TreeCell m v a -> b) -> CellPtr m v a -> m b
readSelector sel ptr = sel <$> readCP ptr

accessRelName :: (Identifier i b, MonadAtomic v m' m) => CellPtr m v a -> m b -> i -> m b




data PropState v = PropState {
  addresses :: forall a b. (Ord a) =>  v (Map a (v b))
}


instance (Dep m v
        , MonadFork m
        , MonadState (PropState v) m
        , MonadNew m v
        , MonadMutate m v
        , MonadRead m v) => MonadProp m (CellPtr m v) where

  read :: (Value a) => CellPtr m v a -> m a
  read ptr = readCP ptr >>= MV.read . value

  write :: (Value a) => CellPtr m v a -> a -> m ()
  write ptr val = do
    cp <- readCP ptr
    hasChanged <- writeLattPtr (value cp) val
    if hasChanged
    then notify ptr
    else return ()

  new :: (Identifier n a, Value a, Std n) => n -> m (CellPtr m v a)
  new name = do
    addr <- gets addresses
    pt <- topTreeCellPtr
    --tries to add the new pointer into the map. If the value at the name already exists, the old one is kept and returned as Just ...
    maybeOld <- MV.mutate addr (flip . Map.insertLookupWithKey (const . const) name pt)
    case maybeOld of
      Just old -> return ()--delete pointer
      Nothing -> return () --TODO: put parent and propagators?



notify :: (MonadRead m v, MonadFork m) => CellPtr m v a -> m ()
notify ptr = do
  propset <- readCPPropagators ptr >>= mapM MV.read . Set.toList
  forkF propset


--
