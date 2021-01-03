{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE ApplicativeDo        #-}
module Control.Propagator.Event.EventT where

import "base" Prelude hiding ( read )
import "base" GHC.Generics
import "base" Data.Typeable
import "base" Data.Functor.Classes
import "base" Data.Type.Equality
import "base" Unsafe.Coerce

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )
import "transformers" Control.Monad.Trans.Class

import "mtl" Control.Monad.Reader.Class

import "this" Control.Propagator.Class
import "this" Control.Propagator.Event.Types
import "this" Data.Typed
import "this" Data.Id


type Evt m = Event (EventT m)

class MonadEvent e m | m -> e where
    fire :: e -> m ()

class MonadRef m where
    getVal :: Identifier i a => Scope -> i -> m a

newtype EventT m a = EventT
    { runEventT :: ReaderT Scope m a
    }
  deriving newtype (Functor, Applicative, Monad, MonadFail)

instance MonadTrans EventT where
    lift = EventT . lift

instance MonadId m => MonadId (EventT m) where
    newId = EventT . ReaderT . const . newId

instance ( Typeable m
         , MonadId m
         , MonadRef m
         , MonadEvent (Evt m) m
         , Monad m
         ) => MonadProp (EventT m) where

    write i a = do
        s <- EventT ask
        lift . fire . WriteEvt $ Write s i a

    watch i j a = do
        s <- EventT ask
        lift . fire . WatchEvt $ Watch s i j a
    
    read i = do
        s <- EventT ask    
        lift $ getVal s i

instance (Monad m, MonadId m, MonadEvent (Evt m) m) => Forkable (EventT m) where
    fork i m = do
        s <- EventT ask
        lift . fire . ForkEvt $ Fork s i m
