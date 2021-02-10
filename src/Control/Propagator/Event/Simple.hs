{-# LANGUAGE StrictData        #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Event.Simple where

import "base" Prelude hiding ( read )
import "base" Data.Foldable
import "base" Data.Functor
import "base" Data.Function ( on )
import "base" Data.Maybe
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
import "mtl" Control.Monad.Reader.Class

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

type SEBState = (Set SEBEvt, Map SEBId (Some Value), Map (Some Std) (Scope, SEB ()))

-------------------------------------------------------------------------------
-- SimpleEventBus
-------------------------------------------------------------------------------

newtype SimpleEventBus a = SEB
    { unSEB :: StateT SEBState IO a
    }
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadState SEBState)

type SEB = EventT SimpleEventBus


runSEB :: SEBState -> Scope -> SEB a -> IO (a, SEBState)
runSEB st sc = flip runStateT st . unSEB . flip runReaderT sc . runEventT

evalSEB :: forall a b. SEB a -> (a -> SEB b) -> IO b
evalSEB start end = fmap fst . runSEB mempty mempty $ do
    a <- start
    go mempty
    end a
  where
    go :: Map (Some Std) (Scope, SEB ()) -> SEB ()
    go pre = do
        flushSEB
        traceM "Reached Fixpoint."
        (_,_, fixpointWatches) <- get
        unless (null fixpointWatches || ((==) `on` Map.keys) pre fixpointWatches) $ do
            forM_ fixpointWatches $ \(s, m) -> local (const s) m
            go fixpointWatches

flushSEB :: SEB ()
flushSEB = do
    (evts, _, _) <- get
    unless (Set.null evts) $ do
        --traceM "flushSEB"
        lift . SEB . modify $ \(_, v, f) -> (mempty, v, f)
        forM_ (Set.toDescList evts) $ \ evt -> do
            --traceM $ show evt
            handleEvent evt
        flushSEB

alterCell :: Identifier i a => Scope -> i -> (Maybe a -> a) -> SEB ()
alterCell s i f = modify $ \(e, v, fw) -> (e, Map.alter f' (SEBId s i) v, fw)
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
handleEvent (WatchEvt (Watch i prop s)) = do
    --handleEvent . WriteEvt . Write (PropagatorsOf @SEB i) [Some p] $ s
    v <- lift $ getValTop s (PropagatorsOf @SEB i)
    case v of
      (newValue -> Just props) -> do
        alterCell s (PropagatorsOf @SEB i) . const $ props
        a <- lift $ getValTop s i
        --forM_ a $ \a' -> execListener s a' (Some prop)
        unless (i =~= Fixpoint) $ execListener s a (Some prop)
      _ -> pure ()
  where
    newValue old = let p' = old /\ (Value $ Some prop) in guard (p' /= old) $> p'
handleEvent (WatchFixpointEvt (WatchFixpoint i m s)) = do
    modify $ \(e, v, f) -> (e, v, Map.insert (Some i) (s, m) f)

val :: Identifier i a => Scope -> i -> SEB a
val s = lift . fmap (fromMaybe Top) . getVal s

notify :: Identifier i a => Scope -> i -> SEB ()
notify s i = do
    a <- val s i
    ls <- val s $ PropagatorsOf @SEB i
    traverse_ (execListener s a) ls

execListener :: Scope -> a -> Some (Propagator SEB a) -> SEB ()
execListener s a (Some p) = local (const s) (propagate p a)

instance MonadEvent (Evt SimpleEventBus) SimpleEventBus where
    fire e = SEB . modify $ \(es, v, f) -> (Set.insert e es, v, f)

instance MonadRef SimpleEventBus where
    getVal s' i = ($ s') . searchCell <$> gets (\(_,x,_) -> x)
      where
        searchCell cx s = lookupCell s cx {- <|> do
            p <- snd <$> popScope s
            searchCell cx p -}
        lookupCell s = (=<<) fromSome . Map.lookup (SEBId s i)
