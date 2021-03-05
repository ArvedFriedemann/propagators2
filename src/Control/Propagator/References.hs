{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.References where

import "base" Prelude hiding ( read )

import "this" Control.MonadVar.MonadVar (MonadNew, MonadMutate, MonadRead)
import qualified "this" Control.MonadVar.MonadVar as MV
import "this" Control.Propagator.Class
import "this" Data.Lattice

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set


data TreeCell m v a = TreeCell {
  parent :: CellPtr m v a
, value :: v a
, children :: v (Map (v Scope) (TreeCell m v a))
, propagators :: CellPtr m v (Set (v (m ())))
}

data CellPtr m v a = CP (v (TreeCell m v a))

unpkCP :: CellPtr m v a -> v (TreeCell m v a)
unpkCP (CP x) = x

readCP :: (MonadRead m v) => CellPtr m v a -> m (TreeCell m v a)
readCP = MV.read . unpkCP

readCPSelector :: (MonadRead m v) => (TreeCell m v a -> v b) -> CellPtr m v a -> m b
readCPSelector sel ptr = (sel <$> readCP ptr) >>= MV.read

readCPValue :: (MonadRead m v) => CellPtr m v a -> m a
readCPValue = readCPSelector value

readCPPropagators :: (MonadRead m v) => CellPtr m v a -> m (Set (v (m ())))
readCPPropagators ptr = (propagators <$> readCP ptr) >>= readCPValue

class Dep a b | a -> b

instance (Dep m v
        , MonadFork m
        , MonadNew m v
        , MonadMutate m v
        , MonadRead m v) => MonadProp m (CellPtr m v) where

  read :: (Value a) => CellPtr m v a -> m a
  read ptr = readCP ptr >>= MV.read . value

  write :: (Value a) => CellPtr m v a -> a -> m ()
  write ptr val = do
    cp <- readCP ptr
    hasChanged <- MV.mutate (value cp) (meetDiff val)
    if hasChanged
    then notify ptr
    else return ()



notify :: (MonadRead m v, MonadFork m) => CellPtr m v a -> m ()
notify ptr = do
  propset <- readCPPropagators ptr >>= mapM MV.read . Set.toList
  forkF propset


class (Monad m) => MonadFork m where
  fork :: m () -> m ()
  forkF :: (Foldable t) => t (m ()) -> m ()
  forkF = foldr (\m t -> fork m >> t) (return ())

--
