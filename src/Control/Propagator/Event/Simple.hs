{-# LANGUAGE StrictData #-}
module Control.Propagator.Event.Simple where

import "base" Data.Foldable
import "base" Data.Maybe
import "base" Control.Applicative
import "base" Control.Monad
import "base" Debug.Trace

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )
import "transformers" Control.Monad.Trans.State.Strict ( StateT(..), evalStateT )
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
    a == b = compare a b == EQ
instance Ord SEBId where
    SEBId s i `compare` SEBId t j = compare s t <> compareTyped i j


type SEBPropagator a = Some (Propagator SEB a)
type SEBCell = Some Value

-------------------------------------------------------------------------------
-- SimpleEventBus
-------------------------------------------------------------------------------

type SEB = EventT SimpleEventBus

data SEBState = SEBS
    { events :: Set Write
    , cells :: Map SEBId SEBCell
    } 
newtype SimpleEventBus a = SEB
    { unSEB :: StateT SEBState IO a
    }
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadState SEBState)

runSEB :: SEB a -> (a -> SEB b) -> IO b
runSEB start end = do
    flip evalStateT (SEBS Set.empty Map.empty) . unSEB . flip runReaderT Root . runEventT $ do
        a <- start
        flushSEB
        end a

flushSEB :: SEB ()
flushSEB = do
    evts <- gets events
    unless (Set.null evts) $ do
        traceM "flushSEB"
        lift . SEB . modify $ \ (SEBS _ cx) -> SEBS Set.empty cx
        forM_ (Set.toDescList evts) $ \ evt -> do
            traceM $ show evt
            handleEvent evt
        flushSEB

alterCell :: (Value a, Identifier i a)
          => Scope -> i 
          -> (Maybe a -> a)
          -> SEB ()
alterCell s i f = modify $ \ st -> st{ cells = Map.alter f' (SEBId s i) . cells $ st }
  where
    f' m = pure . Some . f $ fromSome =<< m

    
handleEvent :: Write -> SEB ()
handleEvent (Write i a s) = do
    v <- lift $ getVal s i
    case v of
        Just old -> do
            let a' =  old /\ a
            unless (a' == old) $ do
                alterCell s i . const $ a'
                notify s i
        Nothing -> alterCell s i . const $ a

execListener :: Scope -> a -> SEBPropagator a -> SEB ()
execListener s a (Some i) = inScope s (propagate i a)

val :: Identifier i a => Scope -> i -> SEB a
val s = lift . fmap (fromMaybe Top) . getVal s

notify :: Identifier i a => Scope -> i -> SEB ()
notify s i = do
    a <- val s i
    ls <- val s (PropagatorsOf @SEB i)
    traverse_ (execListener s a) ls

instance MonadEvent (Evt SimpleEventBus) SimpleEventBus where
    fire (WatchEvt (Watch i p s)) = fire . WriteEvt $ Write (PropagatorsOf @SEB i) [Some p] s
    fire (WriteEvt e) = SEB . modify $ \(SEBS evts cx) -> SEBS (Set.insert e evts) cx

instance MonadRef SimpleEventBus where
    getVal s' i = ($ s') . searchCell <$> gets cells
      where
        searchCell cx s = lookupCell s cx <|> do
            p <- snd <$> popScope s 
            searchCell cx p
        lookupCell s = (=<<) fromSome . Map.lookup (SEBId s i)
