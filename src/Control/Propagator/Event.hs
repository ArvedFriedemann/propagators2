{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData           #-}
module Control.Propagator.Event where

import "base" Data.Typeable
import "base" Data.String
import "base" Data.Type.Equality
import "base" Unsafe.Coerce

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )
import "transformers" Control.Monad.Trans.Class

import "mtl" Control.Monad.Reader.Class

import "this" Control.Propagator.Class
import "this" Data.Id


data Event m where
    Create :: Value a => Id -> a -> Scope m -> Event m
    Write :: Value a => Cell m a -> a -> Scope m -> Event m
    Watch :: Value a => Cell m a -> Id -> (a -> m ()) -> Scope m -> Event m
    Cancel :: Id -> Event m
    Fork :: Scope m -> m () -> Scope m -> Event m

type Evt m = Event (EventT m)

class Monad m => MonadEvent e m | m -> e where
    fire :: e -> m ()

class MonadRef m where
    getVal :: Typeable a => Id -> m a


newtype EventT m a = EventT (ReaderT (Scope (EventT m)) m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans EventT where
    lift = EventT . lift

instance MonadId m => MonadId (EventT m) where
    newId = lift newId

instance TestEquality (Cell (EventT m)) where
    EventCell a `testEquality` EventCell b
        = if a == b
            then Just $ unsafeCoerce Refl
            else Nothing

mkId :: MonadId m => String -> EventT m Id
mkId b = mappend (fromString b) <$> newId

fireEvent :: MonadEvent (Evt m) m
          => (Scope (EventT m) -> Evt m) -> EventT m ()
fireEvent e = lift . fire . e =<< EventT ask


instance ( Typeable m
         , Monad m
         , MonadId m
         , MonadRef m
         , MonadEvent (Evt m) m
         ) => PropagatorMonad (EventT m) where

    newtype Cell (EventT m) a = EventCell
        { cellId :: Id
        }
      deriving stock (Eq, Ord, Show)

    newtype Subscription (EventT m) = Sub
        { subId :: Id
        }
      deriving stock (Eq, Ord, Show)

    newCell i a = do
        i' <- mkId i
        fireEvent $ Create i' a
        pure . EventCell $ i'
        
    namedWatch c i a = do
        i' <- mkId i
        fireEvent $ Watch c i' a
        pure . Subscriptions . pure . Sub $ i'
            
    write c a = fireEvent $ Write c a
    
    readCell = lift . getVal . cellId
    
    cancel = mapM_ (fireEvent . const . Cancel . subId) . getSubscriptions 

nestScope :: Id -> Scope (EventT m) -> Scope (EventT m)
nestScope i (Scope is) = Scope $ i : is

instance ( PropagatorMonad (EventT m)
         , MonadId m
         , Monad m
         ) => Scoped (EventT m) where

    newtype Scope (EventT m) = Scope [Id]
      deriving stock (Eq, Ord, Show)
      deriving newtype (Semigroup, Monoid)

    namedScope n (EventT m) = do
        id' <- mkId n
        EventT . local (nestScope id') $ m

    currentScope = EventT ask
