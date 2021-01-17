{-# LANGUAGE StrictData #-}
module Control.Propagator.Event.Simple where

import "base" Data.Foldable
import "base" Data.Bifunctor
import "base" Data.Functor
import "base" Data.Maybe
import "base" Control.Applicative
import "base" Control.Monad
import "base" Debug.Trace

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )
import "transformers" Control.Monad.Trans.State.Strict ( StateT(..) )
import "transformers" Control.Monad.Trans.Class

import "mtl" Control.Monad.State.Class

import "this" Control.Propagator.Class
import "this" Control.Propagator.Base
import "this" Control.Propagator.Scope
import "this" Control.Propagator.Propagator
import "this" Control.Propagator.Reflection
import "this" Control.Propagator.Event.Types
import "this" Control.Propagator.Event.EventT
import "this" Data.Lattice
import "this" Data.Typed
import "this" Data.Some


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data SEBId where
    SEBId :: Identifier i a => Scope -> i -> SEBId
instance Eq SEBId where
    SEBId s i == SEBId t j = s == t && i =~= j
instance Ord SEBId where
    SEBId s i `compare` SEBId t j = compare s t <> compareTyped i j

type SEBEvt = Evt SimpleEventBus

type SEBState = (Set SEBEvt, Map SEBId (Some Value))

-------------------------------------------------------------------------------
-- SimpleEventBus
-------------------------------------------------------------------------------

newtype SimpleEventBus a = SEB
    { unSEB :: StateT SEBState IO a
    }
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadState SEBState)

type SEB = EventT SimpleEventBus


runSEB :: SEBState -> Scope -> EventT SimpleEventBus a -> IO (a, SEBState)
runSEB st sc = flip runStateT st . unSEB . flip runReaderT sc . runEventT

evalSEB :: SEB a -> (a -> SEB b) -> IO b
evalSEB start end = fmap fst . runSEB mempty mempty $ do
        a <- start
        flushSEB
        end a

flushSEB :: SEB ()
flushSEB = do
    evts <- gets fst
    unless (Set.null evts) $ do
        traceM "flushSEB"
        lift . SEB . modify $ first (const mempty)
        forM_ (Set.toDescList evts) $ \ evt -> do
            traceM $ show evt
            handleEvent evt
        flushSEB

alterCell :: Identifier i a => Scope -> i -> (Maybe a -> a) -> SEB ()
alterCell s i f = modify $ second (Map.alter f' (SEBId s i))
  where f' = pure . Some . f . (=<<) fromSome

handleEvent :: SEBEvt -> SEB ()
handleEvent (WriteEvt (Write i a s)) = do
    v <- lift $ newValue <$> getVal s i
    forM_ v $ \a' -> do
        alterCell s i . const $ a'
        notify s i
  where
    newValue Nothing = Just a
    newValue (Just old) = let a' = old /\ a in guard (a' /= old) $> a'
handleEvent (WatchEvt (Watch i p s)) = do
    handleEvent . WriteEvt . Write (PropagatorsOf @SEB i) [Some p] $ s
    a <- lift $ getVal s i
    forM_ a $ \a' -> execListener s a' (Some p)

val :: Identifier i a => Scope -> i -> SEB a
val s = lift . fmap (fromMaybe Top) . getVal s

notify :: Identifier i a => Scope -> i -> SEB ()
notify s i = do
    traceM $ "notifying " ++ show i ++ " in "++ show s
    a <- val s i
    ls <- val s $ PropagatorsOf @SEB i
    traverse_ (execListener s a) ls

execListener :: Scope -> a -> Some (Propagator SEB a) -> SEB ()
execListener s a (Some p) = inScope s (propagate p a)

instance MonadEvent (Evt SimpleEventBus) SimpleEventBus where
    fire e = SEB . modify $ first (Set.insert e)

instance MonadRef SimpleEventBus where
    getVal s' i = ($ s') . searchCell <$> gets snd
      where
        searchCell cx s = lookupCell s cx {- <|> do
            p <- snd <$> popScope s
            searchCell cx p -}
        lookupCell s = (=<<) fromSome . Map.lookup (SEBId s i)
