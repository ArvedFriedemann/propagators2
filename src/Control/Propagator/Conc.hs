{-# LANGUAGE UndecidableInstances #-}
module Control.Propagator.Conc where

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

import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Iso
import "this" Data.Var


data CellVal m a where
    Val :: !a -> !(Listeners m a) -> ![Mapping m a] -> CellVal m a
    Ref :: (Meet b, Ord b)
        => !(Cell m b) -> !(b <-> a) -> CellVal m a
  
val :: a -> CellVal m a
val a = Val a pool []

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
    { runConcP :: IO a
    }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Ord (Cell ConcPropagator a) where
    compare = compare `on` cellName
  
instance Show (Cell ConcPropagator a) where
    show = liftA2 (\ n i -> n ++ '@' : (show . hashUnique $ i)) cellName cellId

(=~~=) :: Cell ConcPropagator a -> Cell ConcPropagator b -> Maybe (a :~: b)
a =~~= b
    | cellId a == cellId b = pure $ unsafeCoerce Refl
    | otherwise            = Nothing

readCellIO :: Cell ConcPropagator a -> IO a
readCellIO = (readCell' =<<) . readIORef . cellVar
  where
    readCell' (Val v _ _) = pure v
    readCell' (Ref c i)   = to i <$> readCellIO c


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

    readCell = ConcP . readCellIO

    write c a' = mapCell write' c
      where
        write' r@(Ref c' i) = (r, write c' $ from i a')
        write' v@(Val a ls ms) = let a'' = a /\ a' in
            if a == a''
            then (v, pure ())
            else let v' = Val a'' ls ms in
                (v', mapM_  (ConcP . forkListener c) ls)
    
    watch rootC rootL = do
        dirty <- ConcP . liftIO . newIORef $ True
        mapCell (watch' dirty rootC rootL) rootC
      where
        watch' :: IORef Bool -> Cell ConcPropagator a -> (a -> ConcPropagator ())
               -> CellVal ConcPropagator a -> (CellVal ConcPropagator a, ConcPropagator (Subscription ConcPropagator))
        watch' dirty _ l r@(Ref c i) = (r, mapCell (watch' dirty c $ l . to i) c)
        watch' dirty c l (Val a ls ms)
            = let li = Listener dirty l; (i, ls') = newVar li ls
              in (Val a ls' ms,) . ConcP $ do
                  forkIO $ execListener c li
                  pure . ConcPSub . pure $ Sub c i
    
    cancel = mapM_ cancel' . getSubs
      where
        cancel' (Sub c l) = mapCell cancelCell c
          where
            cancelCell (Val a ls ms) = (Val a (delVar l ls) ms, clean $ getVar l ls)
            cancelCell r@(Ref refC _) = (r, mapCell cancelRef refC)

            cancelRef :: CellVal ConcPropagator a
                      -> (CellVal ConcPropagator a, ConcPropagator ())
            cancelRef r@(Ref refC _) = (r, mapCell cancelRef refC)
            cancelRef (Val a ls ms) = (Val a ls $ cancelMapping =<< ms, pure ())

            cancelMapping :: Mapping ConcPropagator a -> [Mapping ConcPropagator a]
            cancelMapping m@(Mapping c' i lx) = case c =~~= c' of
                Just Refl -> pure . Mapping c' i $ delVar l lx
                Nothing   -> pure m

            clean (Listener dirty _) = ConcP . liftIO . writeIORef dirty $ False

mapCell :: (CellVal ConcPropagator a -> (CellVal ConcPropagator a, ConcPropagator b))
        -> Cell ConcPropagator a -> ConcPropagator b
mapCell f = join . ConcP . liftIO . flip atomicModifyIORef f . cellVar

forkListener :: Cell ConcPropagator a -> Listener ConcPropagator a -> IO ()
forkListener c l@(Listener dirty _) = do
    writeIORef dirty True
    void . forkIO $ do
        d <- readIORef dirty
        when d $ execListener c l

execListener :: Cell ConcPropagator a -> Listener ConcPropagator a -> IO ()
execListener c (Listener dirty l) = do
    writeIORef dirty False
    a <- readCellIO c
    runConcP $ l a
