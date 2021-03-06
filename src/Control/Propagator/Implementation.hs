{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Implementation where

import "base" Prelude hiding ( read )
import "base" Data.Tuple
import "base" Data.Typeable
import "base" Data.Unique
import "base" Debug.Trace
import "base" Control.Monad
import "base" Control.Concurrent
import "base" System.IO.Unsafe
import "base" Control.Applicative

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

--IOSTMProp STM Ref
newtype IOSTMProp a = ISP (ReaderT (PropArgs IOSTMProp STM Ref) IO a)
  --deriving (Show,Eq,Ord)


data Ref a = Ref Unique (TVar a)
  deriving (Eq, Typeable)
instance Show (Ref a) where
  show (Ref u _) = show $ hashUnique u
instance Ord (Ref a) where
  compare (Ref u1 _) (Ref u2 _) = compare u1 u2

rvar :: Ref a -> TVar a
rvar (Ref _ v) = v

toRef :: TVar a -> IO (Ref a)
toRef v = newUnique >>= \u -> return $ Ref u v

instance Dep IOSTMProp Ref

instance Functor IOSTMProp where
  fmap fkt (ISP m) = ISP (fmap fkt m)
instance Applicative IOSTMProp where
  pure x = ISP (pure x)
  (ISP m1) <*> (ISP m2) = ISP (m1 <*> m2)
instance Monad IOSTMProp where
  (ISP m) >>= fkt = ISP $ m >>= (\v -> case fkt v of
                                          ISP m' -> m')

instance MonadReader (PropArgs IOSTMProp STM Ref) IOSTMProp where
  ask = ISP ask
  local fkt (ISP m) = ISP (local fkt m)

instance MonadFork IOSTMProp where
  fork (ISP act) = ISP $ do
    s <- ask
    sema <- reader fixpointSemaphore
    void $ lift $ increaseSema sema >> forkIO ((runReaderT act s >> decreaseSema sema) <|> (putStrLn "\nAn error occurred!\n" >> decreaseSema sema))

instance MonadFork IO where
  fork = void . forkIO

class MonadWait m where
  wait :: Int -> m ()
instance MonadWait IOSTMProp where
  wait s = ISP $ lift $ threadDelay s
instance MonadWait IO where
  wait = threadDelay


instance MonadNew IO Ref where
  new a = newTVarIO a >>= toRef
instance MonadRead IO Ref where
  read v = readTVarIO (rvar v)
instance MonadWrite IO Ref where
  write v a = STM.atomically $ writeTVar (rvar v) a
instance MonadMutate_ IO Ref where
  mutate_ v a = STM.atomically $ modifyTVar (rvar v) a
instance MonadMutate IO Ref where
  mutate v fkt = STM.atomically $ stateTVar (rvar v) (swap . fkt)


instance MonadNew IOSTMProp Ref where
  new a = ISP $ lift $ MV.new a
instance MonadRead IOSTMProp Ref where
  read v = ISP $ lift $ MV.read v
instance MonadWrite IOSTMProp Ref where
  write v a = ISP $ lift $ MV.write v a
instance MonadMutate_ IOSTMProp Ref where
  mutate_ v a = ISP $ lift $ MV.mutate_ v a
instance MonadMutate IOSTMProp Ref where
  mutate v fkt = ISP $ lift $ MV.mutate v fkt

instance MonadNew STM Ref where
  new a = newTVar a >>= \v -> return $ unsafePerformIO (toRef v)
instance MonadRead STM Ref where
  read v = readTVar (rvar v)
instance MonadWrite STM Ref where
  write v a = writeTVar (rvar v) a
instance MonadMutate_ STM Ref where
  mutate_ v a = modifyTVar (rvar v) a
instance MonadMutate STM Ref where
  mutate v fkt = stateTVar (rvar v) (swap . fkt)


instance MonadAtomic Ref STM IOSTMProp where
  atomically act = ISP $ lift $ STM.atomically act


runMonadPropIO :: forall a. IOSTMProp a -> IO a
runMonadPropIO act = runMonadPropIOFin act (const $ return ())

runMonadPropIOFin:: forall a b. IOSTMProp a -> (a -> IOSTMProp b) -> IO a
runMonadPropIOFin act fin = do
  initPtrs <- MV.new @_ @Ref Map.empty
  initCreatScopes <- MV.new Map.empty
  root <- MV.new $ ScopeT{createdPointers = initPtrs, createdScopes = initCreatScopes}
  fixActs <- MV.new Map.empty
  fixSema <- MV.new 1
  finalSema <- MV.new 1
  let state = PropArgs{scopePath=[SP root], fixpointActions=fixActs, fixpointSemaphore=fixSema} in do
    res <- runMonadPropIOState state act
    busyFixpointWaiter (-1) finalSema fixSema fixActs (void $ runMonadPropIOState state (fin res))
    decreaseSema fixSema
    let waitFinal = (do
          s <- MV.read finalSema
          if s == 0
          then return ()
          else wait 100 >> waitFinal)
    waitFinal
    return res

runMonadPropIOState :: forall a. PropArgs IOSTMProp STM Ref -> IOSTMProp a -> IO a
runMonadPropIOState state (ISP act) = runReaderT (act) state

busyFixpointWaiter :: (Show a, Eq a) => Int -> Ref Int -> Ref Int -> Ref (Map a (PropArgs IOSTMProp STM Ref,IOSTMProp ())) -> IO () -> IO ()
busyFixpointWaiter j finSema sema fixActs fin = fork (act j)
  where
    act 0 = putStrLn "Waiter Timeout..."
    act i = do
                val <- MV.read sema
                if val <= 0
                then do
                  putStrLn $ "Reached Fixpoint!"
                  acts <- MV.read fixActs
                  if Map.null acts
                  then do
                    putStrLn "No more Fixpoint Actions!"
                    --wait 1000
                    --acts' <- MV.read fixActs
                    --unless (null acts') $ error "Fix acts not null after wait!"
                    fin
                    decreaseSema finSema
                  else do
                    --STM.atomically $ do
                      --acts'<- MV.read fixActs
                      --unless (length acts'== length acts) $ error "fixActs changed when they should not!"
                      --MV.write fixActs Map.empty
                    MV.write fixActs Map.empty
                    forM_ (Map.elems acts) (\(state, m) -> do
                      increaseSema sema
                      fork $ (runMonadPropIOState state m >> decreaseSema sema) <|> (traceM "\nan error occurred!\n" >> decreaseSema sema))
                    act (i-1)

                else do
                  when (i `mod` 100 == 0) $ putStrLn $ "Waiting..."
                  wait 100
                  act (i-1)

increaseSema :: (MonadMutate_ m v) => v Int -> m ()
increaseSema sema = do
  --traceM "Increasing Sema"
  MV.mutate_ sema (\x -> x + (1::Int))

decreaseSema :: (MonadMutate_ m v) => v Int -> m ()
decreaseSema sema = do
  --traceM "Decreasing Sema"
  MV.mutate_ sema (\x -> x - (1::Int))





  --
