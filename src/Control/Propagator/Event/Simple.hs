{-# LANGUAGE StrictData #-}
module Control.Propagator.Event.Simple where

import "base" Data.Typeable
import "base" Data.Maybe
import "base" Data.Foldable
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

import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.State.Class

import "this" Control.Propagator.Class
import "this" Control.Propagator.Event.Types
import "this" Control.Propagator.Event.EventT
import "this" Data.Lattice
import "this" Data.Typed


-------------------------------------------------------------------------------
-- SEBId
-------------------------------------------------------------------------------

data SEBId where
    SEBId :: Identifier i a => Scope -> i -> SEBId
instance Eq SEBId where
    a == b = compare a b == EQ
instance Ord SEBId where
    SEBId s i `compare` SEBId t j = compare s t <> compareTyped i j

-------------------------------------------------------------------------------
-- SEBCell
-------------------------------------------------------------------------------

type SEBPropagator = SomePropagator SEB
data SEBCell where
    SEBC :: Value a => a -> Set (SEBPropagator a) -> SEBCell
instance Show SEBCell where
    showsPrec d (SEBC a m)
        = showParen (d >= 10)
        $ showString "SEBC "
        . showsPrec 10 a
        . showString " "
        . shows (Set.toList m)

toVal :: Value a => SEBCell -> Maybe a
toVal (SEBC a _) = cast a

-------------------------------------------------------------------------------
-- SimpleEventBus
-------------------------------------------------------------------------------

type SEB = EventT SimpleEventBus

data SEBState = SEBS
    { events :: Set (Evt SimpleEventBus)
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

alterCell :: Identifier i a
          => Scope -> i 
          -> (Maybe (a, Set (SEBPropagator a)) -> (a, Set (SEBPropagator a)))
          -> SEB ()
alterCell s i f = modify $ \ st -> st{ cells = Map.alter f' (SEBId s i) . cells $ st }
  where
    f' Nothing = pure . uncurry SEBC . f $ Nothing
    f' (Just (SEBC a ls)) = pure . uncurry SEBC . f $ cast (a, ls)

    
handleEvent :: Evt SimpleEventBus -> SEB ()
handleEvent (WriteEvt (Write i a s)) = do
    v <- getCell s i
    case v of
        Just (old, ls) -> do
            let a' =  old /\ a
            unless (a' == old) $ do
                alterCell s i . setValue $ a'
                mapM_ (execListener s a') ls
        Nothing -> alterCell s i . setValue $ a
  where
    setValue a' = (a',) . maybe [] snd
handleEvent (WatchEvt (Watch c i s)) = do
    v <- fromMaybe top <$> lift (getVal s c)
    alterCell s c . addListener $ v
    execListener s v $ SomePropagator i
  where
    addListener v cv =
        ( maybe v fst cv
        , SomePropagator i `Set.insert` maybe Set.empty snd cv
        )
handleEvent (ForkEvt (Fork i s)) = void . inScope (Scope i s) $ inFork i (inScope s)

inScope :: Scope -> (forall a. SEB a -> SEB a)
inScope = local . const

execListener :: Scope -> a -> SEBPropagator a -> SEB ()
execListener s a (SomePropagator i) = inScope s (propagate i a)

getCell :: (MonadState SEBState m, Identifier i a) => Scope -> i -> m (Maybe (a, Set (SEBPropagator a)))
getCell s' i' = gets $ searchCell' s' i' . cells
  where
    searchCell' Root i cx = getCell' Root i cx
    searchCell' s@(Scope _ p) i cx = getCell' s i cx <|> searchCell' p i cx
    getCell' s i cx = do
        SEBC a lsA <- Map.lookup (SEBId s i) cx
        liftA2 (,) (cast a) (cast lsA)

instance MonadEvent (Evt SimpleEventBus) SimpleEventBus where
    fire e = SEB . modify $ \(SEBS evts cx) -> SEBS (Set.insert e evts) cx

instance MonadRef SimpleEventBus where
    getVal s i = fmap fst <$> getCell s i
