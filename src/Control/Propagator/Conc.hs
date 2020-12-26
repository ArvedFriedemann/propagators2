{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude    #-}
module Control.Propagator.Conc where

import "base" Prelude hiding ( (.), id )
import "base" System.IO.Unsafe
import "base" Data.Function ( on )
import "base" Data.Unique
import "base" Data.IORef
import "base" Data.Type.Equality
import "base" Unsafe.Coerce
import "base" Control.Applicative
import "base" Control.Monad
import "base" Control.Concurrent
import "base" Control.Monad.IO.Class
import "base" Control.Category

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )

import "mtl" Control.Monad.Trans (lift)
import "mtl" Control.Monad.Reader.Class

import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Iso
import "this" Data.Var


data CellVal m a where
    Val :: !a -> !(Listeners m a) -> CellVal m a

val :: a -> CellVal m a
val a = Val a pool

data Listener m a = Listener !(IORef Bool) !(a -> m ())
type Listeners m a = Pool (Listener m a)
type ListenerId m a = Id (Pool (Listener m a)) (Listener m a)

data Mapping m a where
    Mapping :: !(Cell m b) -> !(b <-> a) -> !(Listeners m b) -> Mapping m a

data Sub m where
    Sub :: forall a m. Show (Cell m a)
        => !(Cell m a) -> !(ListenerId m a) -> Sub m

instance Show (Sub m) where
    showsPrec d (Sub c l)
        = showParen (d > 10)
        $ shows c
        . showString " -> "
        . shows l

instance Eq (Sub m) where
    (==) = (==) `on` show
instance Ord (Sub m) where
    compare = compare `on` show


newtype ConcPropagator a = ConcP
    { runConcP :: ReaderT (IORef Int) IO a
    }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)

execConcProp :: ConcPropagator a -> (a -> ConcPropagator b) -> IO b
execConcProp (ConcP setup)  doneP {-(ConcP done)-} = do
    jobCount <- newIORef 0
    a <- runReaderT setup jobCount
    waitForDone jobCount
    (ConcP done) <- return $ doneP a
    runReaderT done jobCount
  where
    waitForDone jobCount = do
        threadDelay 1000000 -- one second
        jobs <- readIORef jobCount
        unless (jobs == 0) $ waitForDone jobCount

instance Ord (Cell ConcPropagator a) where
    compare = compare `on` cellName

instance Show (Cell ConcPropagator a) where
    show = liftA2 (\ n i -> n ++ '@' : (show . hashUnique $ i)) cellName cellId

(=~~=) :: Cell ConcPropagator a -> Cell ConcPropagator b -> Maybe (a :~: b)
a =~~= b
    | cellId a == cellId b = pure $ unsafeCoerce Refl
    | otherwise            = Nothing

readCellValIO :: Cell ConcPropagator a -> ConcPropagator (CellVal ConcPropagator a)
readCellValIO  c = ConcP $ lift $ readIORef $ cellVar c

readCellIO :: Cell ConcPropagator a -> IO a
readCellIO = fmap (\ (Val v _) -> v) . readIORef . cellVar

instance PropagatorMonad ConcPropagator where
    data Cell ConcPropagator a = ConcPCell
        { cellName :: String
        , cellId :: Unique
        , cellVar :: IORef (CellVal ConcPropagator a)
        }
      deriving Eq

    newtype Subscription ConcPropagator = ConcPSub
        { getSubs :: [Sub ConcPropagator]
        }
      deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

    newCell n a = ConcPCell n (unsafePerformIO newUnique) <$> (ConcP . liftIO . newIORef . val $ a)

    readCell = ConcP . liftIO . readCellIO

    write c a' = mapCell write' c
      where
        write' v@(Val a ls) = let a'' = a /\ a' in
            if a == a''
            then (v, pure ())
            else let v' = Val a'' ls in
                (v', mapM_  (ConcP . forkListener c) ls)

    watch rootC rootL = do
        dirty <- ConcP . liftIO . newIORef $ True
        mapCell (watch' dirty rootC rootL) rootC
      where
        watch' :: IORef Bool -> Cell ConcPropagator a -> (a -> ConcPropagator ())
               -> CellVal ConcPropagator a -> (CellVal ConcPropagator a, ConcPropagator (Subscription ConcPropagator))
        watch' dirty c l (Val a ls)
            = let li = Listener dirty l; (i, ls') = newVar li ls
              in (Val a ls',) . ConcP $ do
                    done <- newJob
                    jobCount <- ask
                    liftIO $ do
                        forkIO $ (execListener jobCount c li >> done)
                        pure . ConcPSub . pure $ Sub c i

    cancel = mapM_ cancel' . getSubs
      where
        cancel' (Sub c l) = mapCell cancelCell c
          where
            cancelCell (Val a ls) = (Val a (delVar l ls), clean $ getVar l ls)

            clean (Listener dirty _) = ConcP . liftIO . writeIORef dirty $ False

mapCell :: (CellVal ConcPropagator a -> (CellVal ConcPropagator a, ConcPropagator b))
        -> Cell ConcPropagator a -> ConcPropagator b
mapCell f = join . ConcP . liftIO . flip atomicModifyIORef f . cellVar

changeCell :: Cell ConcPropagator a -> CellVal ConcPropagator a ->  ConcPropagator ()
changeCell cell v = mapCell (\_ -> (v, pure ())) cell

forkListener :: Cell ConcPropagator a -> Listener ConcPropagator a -> ReaderT (IORef Int) IO ()
forkListener c l@(Listener dirty _) = do
    done <- newJob
    jobCount <- ask
    liftIO $ writeIORef dirty True
    void . liftIO . forkIO $ do
        d <- readIORef dirty
        when d $ execListener jobCount c l
        done

execListener :: IORef Int -> Cell ConcPropagator a -> Listener ConcPropagator a -> IO ()
execListener jobCount c (Listener dirty l) = do
    writeIORef dirty False
    a <- readCellIO c
    flip runReaderT jobCount . runConcP $ l a

newJob :: (MonadReader (IORef Int) m, MonadIO m) => m (IO ())
newJob = do
    jobCount <- ask
    liftIO . atomicModifyIORef' jobCount $ (,()) . succ
    pure . atomicModifyIORef' jobCount $ (,()) . pred


instance PropagatorEqMonad ConcPropagator where
  iso :: forall a b. (Ord a, Meet a, Ord b, Meet b)
      => Cell ConcPropagator a -> Cell ConcPropagator b -> (a <-> b) -> ConcPropagator ()
  iso ca cb i = do
    watch ca $ \ a -> write cb $ to i a
    watch cb $ \ b -> write ca $ from i b
    pure ()
