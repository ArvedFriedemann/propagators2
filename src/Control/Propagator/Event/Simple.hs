{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.Event.Simple where

import "base" Prelude hiding ( read )
import "base" Data.Foldable
import "base" Data.Functor
import "base" Data.Function ( on )
import "base" Data.Maybe
import "base" Control.Monad
--import "base" Debug.Trace

import "unordered-containers" Data.HashSet ( HashSet )
import "unordered-containers" Data.HashSet qualified as Set

import "unordered-containers" Data.HashMap.Strict ( HashMap )
import "unordered-containers" Data.HashMap.Strict qualified as Map

import "hashable" Data.Hashable

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
instance Hashable SEBId where
    hashWithSalt n (SEBId s i) = hashWithSalt n (s, i)

type SEBEvt = Evt SimpleEventBus

data SEBState = SEBState
    { events :: HashSet SEBEvt
    , values :: HashMap SEBId (Some Value)
    , fixpointWatches :: HashMap (Some Std) (Scope, SEB ())
    , dirty :: HashSet SEBId
    }
instance Semigroup SEBState where
    a <> b = SEBState (mOn events) (mOn values) (mOn fixpointWatches) (mOn dirty)
      where
        mOn :: Semigroup a => (SEBState -> a) -> a
        mOn f = on (<>) f a b
instance Monoid SEBState where
    mempty = SEBState mempty mempty mempty mempty

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
    go :: HashMap (Some Std) (Scope, SEB ()) -> SEB ()
    go pre = do
        flushSEB
        --traceM "Reached Fixpoint."
        SEBState{ fixpointWatches } <- get
        unless (null fixpointWatches || ((==) `on` Map.keys) pre fixpointWatches) $ do
            forM_ fixpointWatches $ \(s, m) -> local (const s) m
            go fixpointWatches

flushSEB :: SEB ()
flushSEB = do
    evts <- pollEvents
    unless (Set.null evts) $ do
        forM_ evts handleEvent
        drty <- pollDirty
        forM_ drty $ \(SEBId s i) -> notify s i
        flushSEB

pollEvents :: SEB (HashSet SEBEvt)
pollEvents = state $ \st -> (events st, st{ events = mempty })
{-# INLINE pollEvents #-}

pollDirty :: SEB (HashSet SEBId)
pollDirty = state $ \st -> (dirty st, st{ dirty = mempty })
{-# INLINE pollDirty #-}

alterCell :: Identifier i a => Scope -> i -> (Maybe a -> a) -> SEB ()
alterCell s i f = modify $ \st@SEBState{ values } -> st{ values = Map.alter f' (SEBId s i) values }
  where
    f' = pure . Some . f . (=<<) fromSome
    {-# INLINE f' #-}
{-# INLINE alterCell #-}

markDirty :: Identifier i a => Scope -> i -> SEB ()
markDirty s i = modify $ \st@SEBState{ dirty } -> st{ dirty = Set.insert (SEBId s i) dirty }
{-# INLINE markDirty #-}

handleEvent :: SEBEvt -> SEB ()
handleEvent (WriteEvt (Write i a s)) = do
    v <- lift $ newValue <$> getVal s i
    forM_ v $ \a' -> do
        alterCell s i . const $ a'
        markDirty s i
  where
    newValue Nothing = Just a
    newValue (Just old) = let a' = old /\ a in guard (a' /= old) $> a'
    {-# INLINE newValue #-}
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
    {-# INLINE newValue #-}
handleEvent (WatchFixpointEvt (WatchFixpoint i m s)) = do
    modify $ \st@SEBState{ fixpointWatches } -> st{ fixpointWatches = Map.insert (Some i) (s, m) fixpointWatches }

val :: Identifier i a => Scope -> i -> SEB a
val s = lift . fmap (fromMaybe Top) . getVal s
{-# INLINE val #-}

notify :: Identifier i a => Scope -> i -> SEB ()
notify s i = do
    a <- val s i
    ls <- val s $ PropagatorsOf @SEB i
    traverse_ (execListener s a) ls
{-# INLINE notify #-}

execListener :: Scope -> a -> Some (Propagator SEB a) -> SEB ()
execListener s a (Some p) = local (const s) (propagate p a)
{-# INLINE execListener #-}

instance MonadEvent (Evt SimpleEventBus) SimpleEventBus where
    fire e = SEB . modify $ \st@SEBState{ events } -> st{ events = Set.insert e events }
    {-# INLINE fire #-}

instance MonadRef SimpleEventBus where
    getVal s' i = ($ s') . searchCell <$> gets values
      where
        searchCell cx s = (=<<) fromSome . Map.lookup (SEBId s i) $ cx
    {-# INLINE getVal #-}
