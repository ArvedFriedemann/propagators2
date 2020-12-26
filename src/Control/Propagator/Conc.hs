{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE StaticPointers       #-}
module Control.Propagator.Conc where

import "base" Prelude hiding ( (.), id )
import "base" GHC.Generics ( Generic )
import "base" Data.Function ( on )
import "base" Data.IORef
import "base" Data.Typeable
import "base" Data.Type.Equality
import "base" Unsafe.Coerce
import "base" Control.Applicative
import "base" Control.Monad
import "base" Control.Concurrent
import "base" Control.Monad.IO.Class
import "base" Control.Category
import "base" System.IO.Unsafe

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )

import "mtl" Control.Monad.Reader.Class

import "vector" Data.Vector.Mutable qualified as Vec

import "this" Data.ShowM
import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Iso
import "this" Data.Ref

-------------------------------------------------------------------------------
-- MutList
-------------------------------------------------------------------------------

newtype MutList a  = MkML (IORef (Int, Vec.IOVector a))

newMutList :: IO (MutList a)
newMutList = do
    v <- Vec.new 4
    MkML <$> newIORef (0, v)

addMutList :: a -> MutList a -> IO Int
addMutList a (MkML ref) = atomicModifyIORef' ref (unsafePerformIO . add')
  where
    add' (lastIndex, v) = do
      let l = Vec.length v
      v' <- if lastIndex >= l
            then Vec.grow v (l*2)
            else pure v
      Vec.write v' lastIndex a
      pure ((succ lastIndex, v'), lastIndex)

writeMutList :: Int -> a -> MutList a -> IO (MutList a)
writeMutList i a ml@(MkML ref) = atomicModifyIORef' ref (unsafePerformIO . write')
  where
    write' x@(_, v) = (x, ml) <$ Vec.write v i a

readMutList :: Int -> MutList a -> IO a
readMutList i (MkML ref) = (flip Vec.read i =<<) . fmap snd . readIORef $ ref

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

castM :: (Typeable a, Typeable b, MonadFail m) => a -> m b
castM a = do
    let Just b = cast a
    pure b

-------------------------------------------------------------------------------
-- Listener
-------------------------------------------------------------------------------

data Listener a = Listener
    { listenerDirty  :: IORef Bool
    , listenerAction :: Ref (a -> Par ())
    }

newListener :: Ref (a -> Par ()) -> IO (Listener a)
newListener l = flip Listener l <$> newIORef False

instance Eq (Listener a) where
    a == b = compare a b == EQ
instance Ord (Listener a) where
    compare = compare `on` listenerAction

instance Show (Listener a) where
    showsPrec d (Listener _ ref) = showParen (d >= 10) $ showString "Listener " . shows ref

-------------------------------------------------------------------------------
-- ParState
-------------------------------------------------------------------------------

data ParState = ParState
    { jobCount :: IORef Int
    , cells    :: MutList AnyCellVal
    }

newParState :: IO ParState
newParState = ParState <$> newIORef 0 <*> newMutList

newCellIO :: forall a. Value a => String -> a -> ParState -> IO (Cell Par a)
newCellIO name a s = do
    i <- addMutList undefined . cells $ s
    let idA = PID name i
    cv <- newCellVal idA a
    writeMutList i (AnyCellVal cv) . cells $ s
    pure idA

readCellVal :: Value a => Cell Par a -> Par (CellVal a)
readCellVal idA = MkPar $ toCellVal =<< liftIO . readMutList (cellIndex idA) =<< cells <$> ask

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------

data Sub where
    Sub :: Value a => Cell Par a -> Listener a -> Sub

instance Eq Sub where
    Sub cA lA == Sub cB lB = case testEquality cA cB of
        Just Refl -> cA == cB && lA == lB
        Nothing   -> False

instance Show Sub where
    showsPrec d (Sub c l)
        = showParen (d >= 10)
        $ showString "Sub "
        . showsPrec 10 c
        . showString " "
        . showsPrec 10 l

-------------------------------------------------------------------------------
-- Par
-------------------------------------------------------------------------------

newtype Par a = MkPar
    { runPar :: ReaderT ParState IO a
    }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

execPar :: Par a -> (a -> Par b) -> IO b
execPar = execPar' 1000000 -- one second

execPar' :: Int -> Par a -> (a -> Par b) -> IO b
execPar' tick setup doneP = do
    s <- newParState
    a <- run s setup
    waitForDone s
    run s . doneP $ a
  where
    run s = flip runReaderT s . runPar
    waitForDone s = do
        threadDelay tick
        jobs <- readIORef . jobCount $ s
        unless (jobs == 0) . waitForDone $ s

-- Cell

instance Show (Cell Par a) where
    show (PID n i) = n ++ '@' : show i

instance TestEquality (Cell Par) where
    testEquality a b 
        = if cellIndex a == cellIndex b
          then unsafeCoerce $ Just Refl
          else Nothing

-- PropagatorMonad

instance PropagatorMonad Par where

    data Cell Par a = PID
        { cellName  :: String
        , cellIndex :: Int
        }
      deriving (Eq, Ord, Generic)

    newtype Subscription Par = ConcPSub
        { getSubs :: [Sub]
        }
      deriving newtype (Eq, Show, Semigroup, Monoid)
        
    newCell n a = MkPar $ liftIO . newCellIO n a =<< ask

    readCell = (liftIO . readIORef . getCellVal =<<) . readCellVal

    write c a = do
        cv <- readCellVal c
        changed <- liftIO $ atomicModifyIORef' (getCellVal cv) (meetEq a)
        when changed $ do
            ls <- fmap Set.toList . liftIO . readIORef . listeners $ cv
            mapM_ (forkListener c) ls
    
    watch c l = do
        l' <- liftIO . newListener $ l
        ls <- fmap listeners . readCellVal $ c
        liftIO . atomicModifyIORef' ls $ (,()) . Set.insert l'
        forkListener c l'
        pure . ConcPSub . pure $ Sub c l'
    
    cancel = mapM_ cancel' . getSubs
      where
        cancel' (Sub c l) = do
            liftIO $ writeIORef (listenerDirty l) False
            ls <- fmap listeners . readCellVal $ c
            liftIO . atomicModifyIORef' ls $ (,()) . Set.delete l

instance PropagatorEqMonad Par where
    iso :: forall a b. (Value a, Value b) => Cell Par a -> Cell Par b -> Ref (a <-> b) -> Par ()
    iso ca cb i = do
        watch ca $ (staticWrite $# liftRef cb) .# (staticTo $## i)
        watch cb $ (staticWrite $# liftRef ca) .# (staticFrom $## i)
        pure ()

forkListener :: Value a => Cell Par a -> Listener a -> Par ()
forkListener c (Listener dirty l) = do
    jobs <- jobCount <$> MkPar ask
    liftIO $ writeIORef dirty True
    liftIO . atomicModifyIORef' jobs $ (,()) . succ
    void . MkPar . ReaderT $ \ s -> forkIO $ do
        d <- atomicModifyIORef' dirty (False,)
        when d $ do
            flip runReaderT s . runPar $ deRef l =<< readCell c
        atomicModifyIORef' jobs $ (,()) . pred
