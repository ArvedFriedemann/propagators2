{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE ApplicativeDo        #-}
module Control.Propagator.Event.EventT where

import "base" GHC.Generics
import "base" Data.Typeable
import "base" Data.Functor.Classes
import "base" Data.Type.Equality
import "base" Unsafe.Coerce

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )
import "transformers" Control.Monad.Trans.Class

import "mtl" Control.Monad.Reader.Class

import "deepseq" Control.DeepSeq

import "this" Control.Propagator.Class
import "this" Control.Propagator.Event.Types
import "this" Data.Id


type Evt m = Event (EventT m)

class MonadEvent e m | m -> e where
    fire :: e -> m ()

class MonadRef m where
    getVal :: Value a => Scope -> Id -> m a

newtype EventT m a = EventT
    { runEventT :: ReaderT Scope m a
    }
  deriving newtype (Functor, Applicative, Monad, MonadFail)

instance MonadTrans EventT where
    lift = EventT . lift

instance MonadId m => MonadId (EventT m) where
    newId = EventT . ReaderT . const . newId

instance TestEquality (Cell (EventT m)) where
    EventCell a `testEquality` EventCell b
        = if a == b
            then Just $ unsafeCoerce Refl
            else Nothing

instance Eq1 (Cell (EventT m)) where
    liftEq _ (cellId -> a) (cellId -> b) = a == b
instance Ord1 (Cell (EventT m)) where
    liftCompare _ (cellId -> a) (cellId -> b) = a `compare` b

instance NFData (Cell (EventT m) a)

instance Eq (Subscription (EventT m)) where
    a == b = compare a b == EQ
instance Ord (Subscription (EventT m)) where
    Sub sa ca ia `compare` Sub sb cb ib
        = compare sa sb
        <> compare (cellId ca) (cellId cb)
        <> compare ia ib
deriving instance (forall a. Show (Cell (EventT m) a)) => Show (Subscription (EventT m))

instance NFData (Subscription (EventT m)) where
    rnf (Sub c i s) = c `deepseq` i `deepseq` s `deepseq` ()

instance ( Typeable m
         , MonadId m
         , MonadRef m
         , MonadEvent (Evt m) m
         , Monad m
         ) => PropagatorMonad (EventT m) where

    newtype Cell (EventT m) a = EventCell
        { cellId :: Id
        }
      deriving newtype Show
      deriving stock (Eq, Ord, Generic)

    data Subscription (EventT m) where
        Sub :: Scope -> Cell (EventT m) a -> Id -> Subscription (EventT m)

    newCell i a = do
        c <- EventCell <$> newId i
        s <- EventT ask
        lift . fire . CreateEvt $ Create s c a
        pure c

    namedWatch c i a = do
        i' <- newId i
        s <- EventT ask
        lift . fire . WatchEvt $ Watch s c i' a
        pure . Subscriptions . pure $ Sub s c i'

    write c a = do
        s <- EventT ask
        lift . fire . WriteEvt $ Write s c a

    readCell (cellId -> i) = lift . flip getVal i =<< EventT ask

    cancel = mapM_ (lift . fire . CancelEvt . Cancel) . getSubscriptions


instance (Monad m, MonadId m, MonadEvent (Evt m) m) => Forkable (EventT m) where
    namedFork n m = do
        cur <- EventT ask
        child <- newId n
        lift . fire . ForkEvt $ Fork cur child m
