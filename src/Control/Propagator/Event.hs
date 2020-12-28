{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE ApplicativeDo        #-}
module Control.Propagator.Event where

import "base" Data.Typeable
import "base" Data.String
import "base" Data.Functor.Classes
import "base" Data.Type.Equality
import "base" Unsafe.Coerce

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )
import "transformers" Control.Monad.Trans.Class

import "mtl" Control.Monad.Reader.Class

import "this" Control.Propagator.Class
import "this" Data.Id


data Event m where
    Create :: Value a => Scope -> Id -> a -> Event m
    Write :: Value a => Scope -> Cell m a -> a -> Event m
    Watch :: Value a => Scope -> Cell m a -> Id -> (a -> m ()) -> Event m
    Fork :: Scope -> Scope -> (LiftParent m -> m ()) -> Event m
    Cancel :: Subscription m -> Event m

viaType :: (c a, c TypeRep, Typeable a, Typeable b)
        => Proxy c -> (forall x. c x => x -> x -> y)
        -> a -> b -> y
viaType _ f a b = case cast b of
    Just a' -> f a a'
    Nothing -> typeOf a `f` typeOf b

instance Ord (Event m) => Eq (Event m) where
    a == b = compare a b == EQ
instance (Ord (Scope), Ord1 (Cell m), Ord (Subscription m)) => Ord (Event m) where
    Create sa ia a `compare` Create sb ib b 
        = compare sa sb
        <> compare ia ib
        <> viaType (Proxy @Ord) compare a b
    Write sa ca a `compare` Write sb cb b
        = compare sa sb
        <> liftCompare undefined ca cb
        <> viaType (Proxy @Ord) compare a b
    Watch sa ca ia _ `compare` Watch sb cb ib _
        = compare sa sb
        <> liftCompare undefined ca cb
        <> compare ia ib
    Fork sa csa _ `compare` Fork sb csb _
        = compare sa sb
        <> compare csa csb
    Cancel a `compare` Cancel b = compare a b
    Create _ _ _ `compare` _ = GT
    _ `compare` Create _ _ _ = LT
    Write _ _ _ `compare` _ = GT
    Watch _ _ _ _ `compare` _ = GT
    _ `compare` Watch _ _ _ _ = LT
    _ `compare` Write _ _ _ = LT
    Fork _ _ _ `compare` _ = GT
    _ `compare` Fork _ _ _ = LT

type Evt m = Event (EventT m)

class MonadEvent e m | m -> e where
    fire :: e -> m ()

class MonadRef m where
    getVal :: Typeable a => Id -> m a

newtype EventT m a = EventT
    { runEventT :: ReaderT (Scope) m a
    }
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans EventT where
    lift = EventT . lift

instance MonadId m => MonadId (EventT m) where
    newId = EventT . ReaderT . const $ newId

instance TestEquality (Cell (EventT m)) where
    EventCell a `testEquality` EventCell b
        = if a == b
            then Just $ unsafeCoerce Refl
            else Nothing

mkId :: (Functor m, MonadId m) => String -> EventT m Id
mkId b = mappend (fromString b) <$> newId

instance Eq1 (Cell (EventT m)) where
    liftEq _ (cellId -> a) (cellId -> b) = a == b
instance Ord1 (Cell (EventT m)) where
    liftCompare _ (cellId -> a) (cellId -> b) = a `compare` b
    

instance Eq (Subscription (EventT m)) where
    a == b = compare a b == EQ
instance Ord (Subscription (EventT m)) where
    Sub ca ia sa `compare` Sub cb ib sb
        = compare (cellId ca) (cellId cb)
        <> compare ia ib
        <> compare sa sb
deriving instance (forall a. Show (Cell (EventT m) a)) => Show (Subscription (EventT m))

instance ( Typeable m
         , MonadId m
         , MonadRef m
         , MonadEvent (Evt m) m
         , Monad m
         ) => PropagatorMonad (EventT m) where

    newtype Cell (EventT m) a = EventCell
        { cellId :: Id
        }
      deriving stock (Eq, Ord, Show)

    data Subscription (EventT m) where
        Sub :: Cell (EventT m) a -> Id -> Scope -> Subscription (EventT m)

    newCell i a = do
        i' <- mkId i
        s <- EventT ask
        lift . fire $ Create s i' a
        pure . EventCell $ i'
        
    namedWatch c i a = do
        i' <- mkId i 
        s <- EventT ask
        lift . fire $ Watch s c i' a
        pure . Subscriptions . pure $ Sub c i' s
            
    write c a = do
        s <- EventT ask
        lift . fire $ Write s c a
    
    readCell (cellId -> i) = do
        Scope s <- EventT ask
        lift . getVal $ s <> i
    
    cancel = mapM_ (lift . fire . Cancel) . getSubscriptions 

newtype Scope = Scope Id
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

instance (Monad m, MonadId m, MonadEvent (Evt m) m) => Forkable (EventT m) where
    namedFork n m = do
        cur <- EventT ask
        child <- flip mappend cur . Scope <$> mkId n
        lift . fire $ Fork cur child m
