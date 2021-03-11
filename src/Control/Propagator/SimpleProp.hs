{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.SimpleProp where

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

data PropCell m a = PropCell {
    value :: a
  , propagators :: [m ()]
}

data CellPtr m v a = CP (v (PropCell m a))

unpkCP :: CellPtr m v a -> v (PropCell m a)
unpkCP (CP ptr) = ptr

readCP :: (MonadRead m v) => CellPtr m v a -> m (PropCell m a)
readCP ptr = MV.read $ unpkCP ptr

readSelector :: (MonadRead m v) => (PropCell m a -> b) -> CellPtr m v a -> m b
readSelector sel ptr = sel <$> readCP ptr




instance (Dep m v
        , MonadFork m
        , MonadNew m v
        , MonadMutate m v
        , MonadRead m v) => MonadPropSimple m (CellPtr m v) where

  readS :: CellPtr m v a -> m a
  readS = readSelector value

  writeS :: (Value a) => CellPtr m v a -> a -> m ()
  writeS ptr val = do
    (props, hasChanged) <- MV.mutate (unpkCP ptr)
      (\pc -> let (a,b) = meetDiff val (value pc)
              in (pc{value=a},propagators pc, b))
    when hasChanged $ forkF props

  watchS :: CellPtr m v a -> m () -> m ()
  watchS ptr act = do
    MV.mutate (unpkCP ptr)
      (\pc -> (pc{propagators = act : (propagators pc)},()))
    fork act



--
