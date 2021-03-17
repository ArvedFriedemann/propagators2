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

newtype IOSTMProp a = ISP (ReaderT (PropArgs IOSTMProp STM TVar) IO a)

instance Dep IOSTMProp TVar

instance Functor IOSTMProp where
  fmap fkt (ISP m) = ISP (fmap fkt m)
instance Applicative IOSTMProp where
  pure x = ISP (pure x)
  (ISP m1) <*> (ISP m2) = ISP (m1 <*> m2)
instance Monad IOSTMProp where
  (ISP m) >>= fkt = ISP $ m >>= (\v -> case fkt v of
                                          ISP m' -> m')

instance MonadFork IOSTMProp where
  fork (ISP act) = ISP $ do
    s <- ask
    void $ lift $ forkIO (runReaderT act s)

--ReaderT (PropArgs IO STM TVar)
runMonadPropIO :: (MonadProp IOSTMProp (CellPtr STM TVar) (Scope TVar)) => IO a -> IO a
runMonadPropIO act = do
  initPtrs <- MV.new @_ @TVar Map.empty
  root <- MV.new $ ScopeT{createdPointers = initPtrs}
  creatScopes <- MV.new Map.empty
  fixActs <- MV.new Map.empty
  runReaderT (lift act) (PropArgs{scopePath=[SP root], createdScopes=creatScopes, fixpointActions=fixActs })







  --
