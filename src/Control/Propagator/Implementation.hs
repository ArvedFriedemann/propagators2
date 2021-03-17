{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Implementation where

import "base" Prelude hiding ( read )
import "base" Data.Tuple
import "base" Data.Typeable
import "base" Debug.Trace
import "base" Control.Monad
import "base" Control.Concurrent

import "this" Control.Propagator.References
import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Some

import qualified "transformers" Control.Monad.Trans.Reader as TR
import "transformers" Control.Monad.Trans.Class
import "stm" Control.Concurrent.STM hiding (atomically)
import qualified "stm" Control.Concurrent.STM as STM
import "mtl" Control.Monad.Reader

import "this" Control.MonadVar.MonadVar (MonadNew, MonadMutate, MonadMutate_, MonadWrite, MonadRead)
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

instance MonadReader (PropArgs IOSTMProp STM TVar) IOSTMProp where
  ask = ISP ask
  local fkt (ISP m) = ISP (local fkt m)

instance MonadFork IOSTMProp where
  fork (ISP act) = ISP $ do
    s <- ask
    void $ lift $ forkIO (runReaderT act s)

instance MonadNew IOSTMProp TVar where
  new a = ISP $ lift $ newTVarIO a
instance MonadRead IOSTMProp TVar where
  read a = ISP $ lift $ readTVarIO a
instance MonadWrite IOSTMProp TVar where
  write v a = ISP $ lift $ STM.atomically $ writeTVar v a
instance MonadMutate_ IOSTMProp TVar where
  mutate_ v a = ISP $ lift $ STM.atomically $ modifyTVar v a
instance MonadMutate IOSTMProp TVar where
  mutate v fkt = ISP $ lift $ STM.atomically $ stateTVar v (swap . fkt)
instance MonadAtomic TVar STM IOSTMProp where
  atomically act = ISP $ lift $ STM.atomically act

--ReaderT (PropArgs IO STM TVar)
runMonadPropIO :: (MonadProp IOSTMProp (CellPtr STM TVar) (Scope TVar)) => IO a -> IO a
runMonadPropIO act = do
  initPtrs <- MV.new @_ @TVar Map.empty
  root <- MV.new $ ScopeT{createdPointers = initPtrs}
  creatScopes <- MV.new Map.empty
  fixActs <- MV.new Map.empty
  runReaderT (lift act) (PropArgs{scopePath=[SP root], createdScopes=creatScopes, fixpointActions=fixActs })







  --
