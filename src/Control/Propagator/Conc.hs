{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE StrictData           #-}
module Control.Propagator.Conc
    ( Par
    , execPar
    , execPar'
    ) where

import "base" Prelude hiding ( (.), id )
import "base" GHC.Generics ( Generic )
import "base" Data.Function ( on )
import "base" Data.Unique
import "base" Data.IORef
import "base" Data.Typeable
import "base" Data.Type.Equality
import "base" Unsafe.Coerce
import "base" Control.Applicative
import "base" Control.Monad
import "base" Control.Concurrent
import "base" Control.Category

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )

import "this" Data.ShowM
import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.MutList ( MutList )
import "this" Data.MutList qualified as MutList

-------------------------------------------------------------------------------
-- CellVal
-------------------------------------------------------------------------------

data CellVal a = CellVal
    { cellId     :: Cell Par a
    , getCellVal :: IORef a
    , listeners  :: IORef (Set (Listener a))
    }

newCellVal :: Cell Par a -> a -> IO (CellVal a)
newCellVal idA a = CellVal idA
                <$> newIORef a
                <*> newIORef Set.empty

copyCellVal :: Value a => CellVal a -> IO (CellVal a)
copyCellVal cv = CellVal (cellId cv)
              <$> copyIORef (getCellVal cv)
              <*> copyIORef (listeners cv)

copyIORef :: IORef a -> IO (IORef a)
copyIORef r = newIORef =<< readIORef r

instance Show a => Show (CellVal a) where
    showsPrec d = showsPrec d . cellId

instance Show a => ShowM IO (CellVal a) where
    showsPrecM d cv = do
        let idA = cellId cv
        a <- readIORef . getCellVal $ cv
        ls <- readIORef . listeners $ cv
        pure . showParen (d >= 10)
            $ shows idA
            . showString " = " . showsPrec 0 a
            . showString " listeners=" . (shows . Set.toList $ ls)

instance Eq a => Eq (CellVal a) where
    a == b = on (==) cellId a b && on (==) getCellVal a b

instance Ord a => Ord (CellVal a) where
    compare = compare `on` cellId

data AnyCellVal where
    AnyCellVal :: Value a => CellVal a -> AnyCellVal

toCellVal :: (Value a, MonadFail m) => AnyCellVal -> m (CellVal a)
toCellVal (AnyCellVal ref) = castM ref

copyAnyCellVal :: AnyCellVal -> IO AnyCellVal
copyAnyCellVal (AnyCellVal v) = AnyCellVal <$> copyCellVal v

castM :: (Typeable a, Typeable b, MonadFail m) => a -> m b
castM a = do
    let Just b = cast a
    pure b

-------------------------------------------------------------------------------
-- Listener
-------------------------------------------------------------------------------

data Listener a = Listener Unique (IORef Bool) (a -> Par ())

listenerId :: Listener a -> Unique
listenerId (Listener i _ _) = i

listenerDirty :: Listener a -> IORef Bool
listenerDirty (Listener _ d _) = d


newListener :: (a -> Par ()) -> IO (Listener a)
newListener l = Listener
    <$> newUnique
    <*> newIORef False
    <*> pure l

instance Eq (Listener a) where
    a == b = compare a b == EQ
instance Ord (Listener a) where
    compare = compare `on` listenerId

instance Show (Listener a) where
    showsPrec d (Listener lId _ _) = showParen (d >= 10) $ showString "Listener " . shows (hashUnique lId)

-------------------------------------------------------------------------------
-- ParState
-------------------------------------------------------------------------------

data ParState = ParState
    { jobCount :: IORef Int
    , cells    :: MutList AnyCellVal
    }

newParState :: IO ParState
newParState = ParState <$> newIORef 0 <*> MutList.new

newCellIO :: forall a. Value a => String -> a -> ParState -> IO (Cell Par a)
newCellIO name a s = do
    i <- MutList.add undefined . cells $ s
    let idA = PID name i
    cv <- newCellVal idA a
    MutList.write i (AnyCellVal cv) . cells $ s
    pure idA

readCellValIO :: Value a => Cell Par a -> ParState -> IO (CellVal a)
readCellValIO idA s = toCellVal =<< MutList.read (cellIndex idA) (cells s)

copyParState :: ParState -> IO ParState
copyParState s = ParState (jobCount s)
             <$> (MutList.map copyAnyCellVal . cells $ s)

-------------------------------------------------------------------------------
-- Par
-------------------------------------------------------------------------------

newtype Par a = MkPar
    { runPar' :: ReaderT ParState IO a
    }
  deriving newtype (Functor, Applicative, Alternative, Monad)

execPar :: Par a -> (a -> Par b) -> IO b
execPar = execPar' 100000 -- 100 millsec

execPar' :: Int -> Par a -> (a -> Par b) -> IO b
execPar' tick setup doneP = do
    s <- newParState
    a <- runPar s setup
    waitForDone s
    runPar s . doneP $ a
  where
    waitForDone s = do
        threadDelay tick
        jobs <- readIORef . jobCount $ s
        unless (jobs == 0) . waitForDone $ s

runPar :: ParState -> Par a -> IO a
runPar s = flip runReaderT s . runPar'

-- Cell

instance Show (Cell Par a) where
    show (PID n i) = n ++ '@' : show i

instance TestEquality (Cell Par) where
    testEquality a b 
        = if cellIndex a == cellIndex b
          then unsafeCoerce $ Just Refl
          else Nothing

-- Sub

instance Eq (Subscription Par) where
    Sub cA lA == Sub cB lB = case testEquality cA cB of
        Just Refl -> cA == cB && lA == lB
        Nothing   -> False

instance Show (Subscription Par) where
    showsPrec d (Sub c l)
        = showParen (d >= 10)
        $ showString "Sub "
        . showsPrec 10 c
        . showString " "
        . showsPrec 10 l

-- PropagatorMonad

instance PropagatorMonad Par where

    data Cell Par a = PID
        { cellName  :: String
        , cellIndex :: Int
        }
      deriving (Eq, Ord, Generic)

    data Subscription Par where
        Sub :: Value a => Cell Par a -> Listener a -> Subscription Par

    newCell n = liftPar . newCellIO n
    readCell = liftPar . readCellIO
    write c = liftPar . writeIO c
    namedWatch c _ = liftPar . watchIO c
    cancel = liftPar . cancelIO

instance Forkable Par where
    namedFork _ m = liftPar $ forkParIO m

-- IO

liftPar :: (ParState -> IO a) -> Par a
liftPar = MkPar . ReaderT

readCellIO :: Value a => Cell Par a -> ParState -> IO a
readCellIO c s = readIORef . getCellVal =<< readCellValIO c s 

cancelIO :: Subscriptions Par -> ParState -> IO ()
cancelIO (getSubscriptions -> subs) s = mapM_ cancel' subs
  where
    cancel' (Sub c l) = do
        writeIORef (listenerDirty l) False
        ls <- listeners <$> readCellValIO c s
        atomicModifyIORef' ls $ (,()) . Set.delete l

writeIO :: Value a => Cell Par a -> a -> ParState -> IO ()
writeIO c a s = do
    cv <- readCellValIO c s
    changed <- atomicModifyIORef' (getCellVal cv) (meetEq a)
    when changed $ do
        ls <- fmap Set.toList . readIORef . listeners $ cv
        mapM_ (flip (forkListenerIO c) s) ls

watchIO :: Value a => Cell Par a -> (a -> Par ()) -> ParState -> IO (Subscriptions Par)
watchIO c l s = do
    l' <- newListener $ l
    ls <- fmap listeners . readCellValIO c $ s
    atomicModifyIORef' ls $ (,()) . Set.insert l'
    forkListenerIO c l' s
    pure . Subscriptions . pure $ Sub c l'

forkListenerIO :: Value a => Cell Par a -> Listener a -> ParState -> IO ()
forkListenerIO c (Listener _ dirty l) s = do
    writeIORef dirty True
    forkJob s $ do
        d <- atomicModifyIORef' dirty (False,)
        when d $ runPar s . l =<< readCellIO c s

forkParIO :: (LiftParent Par -> Par ()) -> ParState -> IO () 
forkParIO m s = do
    s' <- copyParState s
    connectStates s s'
    forkJob s (runPar s' $ m (liftParent s))

liftParent :: ParState -> LiftParent Par
liftParent s a = liftPar $ \ _ -> runPar s a

connectStates :: ParState -> ParState -> IO ()
connectStates s s' = mapM_ connectCell =<< enumFromTo 0 <$> pred <$> MutList.length (cells s)
  where
    connectCell i = do
        AnyCellVal c <- flip MutList.read (cells s) i
        let idC = cellId c
        watchIO idC (\ v -> liftPar $ \ _ ->  writeIO idC v s') s

forkJob :: ParState -> IO a -> IO ()
forkJob s m = do
    let jobs = jobCount s
    let modJobs f = atomicModifyIORef' jobs $ (,()) . f
    modJobs succ
    void . forkIO $ m >> modJobs pred
