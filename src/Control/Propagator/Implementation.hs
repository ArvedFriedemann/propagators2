{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Implementation where

import "base" Prelude hiding ( read )
import "base" Data.Typeable
import "base" Debug.Trace
import "base" Control.Monad
import "base" Control.Concurrent

import "this" Control.Propagator.References
import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Some

import "transformers" Control.Monad.Trans.Reader
import "transformers" Control.Monad.Trans.Class
import "stm" Control.Concurrent.STM

import "this" Control.MonadVar.MonadVar (MonadNew, MonadMutate, MonadRead)
import qualified "this" Control.MonadVar.MonadVar as MV

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

instance Dep IO TVar

instance MonadFork IO where
  fork = void . forkIO

--ReaderT (PropArgs IO STM TVar)
runMonadPropIO :: (MonadProp (ReaderT (PropArgs IO STM TVar) IO) (CellPtr STM TVar) (Scope TVar)) => IO a -> IO a
runMonadPropIO act = do
  initPtrs <- MV.new @_ @TVar Map.empty
  root <- MV.new $ ScopeT{createdPointers = initPtrs}
  creatScopes <- MV.new Map.empty
  fixActs <- MV.new Map.empty
  runReaderT (lift act) (PropArgs{scopePath=[SP root], createdScopes=creatScopes, fixpointActions=fixActs })







  --
