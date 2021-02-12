{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
module Control.Propagator.Event.EventT
    ( Evt
    , EventT(..)
    , MonadEvent(..)
    , MonadRef(..)
    ) where

import "base" Prelude hiding ( read )
import "base" Data.Maybe
import "base" Data.Typeable

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )
import "transformers" Control.Monad.Trans.Class

import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.State.Class

import "this" Control.Propagator.Base
import "this" Control.Propagator.Scope
import "this" Control.Propagator.Event.Types
import "this" Control.Propagator.Combinators ( request )
import "this" Data.Lattice



type Evt m = Event (EventT m)

class Monad m => MonadEvent e m | m -> e where
    fire :: e -> m ()

class Monad m => MonadRef m where
    getVal :: Identifier i a => Scope -> i -> m (Maybe a)
    getValTop :: (Identifier i a, BoundedJoin a) => Scope -> i -> m a
    getValTop s i = fromMaybe top <$> getVal s i

newtype EventT m a = EventT
    { runEventT :: ReaderT Scope m a
    }
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadReader Scope)
deriving newtype instance MonadState s m => MonadState s (EventT m)

instance MonadTrans EventT where
    lift = EventT . lift
    {-# INLINE lift #-}

withScope :: Monad m => (Scope -> m a) -> EventT m a
withScope f = ask >>= lift . f
{-# INLINE withScope #-}

fire' :: MonadEvent (Evt m) m => (Scope -> Evt m) -> EventT m ()
fire' ctr = withScope $ fire . ctr
{-# INLINE fire' #-}

instance (Typeable m, MonadRef m, MonadEvent (Evt m) m, Monad m) => MonadProp (EventT m) where

    write i a = do
      --read i
      i <$ (fire' $ WriteEvt . Write i a)
    {-# INLINE write #-}

    watch i p = do
      read i
      i <$ (fire' $ WatchEvt . Watch i p)
    {-# INLINE watch #-}

    read i = do
      request i
      --request (PropagatorsOf @(EventT m) i)
      fmap (fromMaybe Top) . withScope . flip getVal $ i
    {-# INLINE read #-}

    parScoped m = do
        s <- ask
        case s of
            (s' :/ i) -> pure <$> (local (const s') $ m i)
            _ -> pure Nothing
    {-# INLINE parScoped #-}

    scoped i = local (:/ i)
    {-# INLINE scoped #-}

    watchFixpoint i m = fire' $ WatchFixpointEvt . WatchFixpoint i m
    {-# INLINE watchFixpoint #-}
