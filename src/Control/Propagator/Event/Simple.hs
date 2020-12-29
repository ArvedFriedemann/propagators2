{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE ApplicativeDo        #-}
module Control.Propagator.Event.Simple where

import "base" GHC.Generics
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
import "this" Data.Id


data SEBId = SEBId Scope Id
  deriving (Eq, Ord, Show, Generic)

data SEBCell where
    SEBC :: Value a => a -> Map Id (a -> SEB ()) -> SEBCell
instance Show SEBCell where
    showsPrec d (SEBC a m)
        = showParen (d >= 10)
        $ showString "SEBC "
        . showsPrec 10 a
        . showString " "
        . shows (Map.keys m)

toVal :: Value a => SEBCell -> Maybe a
toVal (SEBC a _) = cast a

type SEB = EventT SimpleEventBus

data SEBState = SEBS
    { unSEBS :: Set (Evt SimpleEventBus)
    , cells :: Map SEBId SEBCell
    } 
newtype SimpleEventBus a = SEB
    { unSEB :: StateT SEBState IO a
    }
  deriving newtype (Functor, Applicative, Monad, MonadFail)

instance MonadId SimpleEventBus where
    newId = SEB . lift . newId

runSEB :: SEB a -> (a -> SEB b) -> IO b
runSEB start end = do
    root <- Scope . pure <$> newId "root"
    flip evalStateT (SEBS Set.empty Map.empty) . unSEB . flip runReaderT root . runEventT $ do
        a <- start
        flushSEB
        end a

flushSEB :: SEB ()
flushSEB = do
    evts <- lift . SEB . gets $ unSEBS
    unless (Set.null evts) $ do
        traceM "flushSEB"
        lift . SEB . modify $ \ (SEBS _ cx) -> SEBS Set.empty cx
        forM_ (Set.toDescList evts) $ \ evt -> do
            traceM $ show evt
            handleEvent evt
        flushSEB
    
handleEvent :: Evt SimpleEventBus -> SEB ()
handleEvent (CreateEvt (Create s i a)) = lift $ do
    SEB . modify $ \ (SEBS evt cx) -> SEBS evt $ Map.insert (SEBId s $ cellId i) (SEBC a Map.empty) cx
handleEvent (WriteEvt (Write s i a)) = do
    Just (old, ls) <- searchCell s i . cells <$> lift (SEB get) 
    let a' =  old /\ a
    unless (a' == old) $ do
        lift . SEB . modify $ \ (SEBS evt cx) -> SEBS evt $ Map.adjust (setValue a') (SEBId s $ cellId i) cx
        mapM_ (execListener s a') ls
  where
    setValue a' (SEBC _ ls) = SEBC (fromJust $ cast a') ls
handleEvent (WatchEvt (Watch s c i act)) = do
    Just (v, _) <- searchCell s c . cells <$> lift (SEB get)
    lift . SEB . modify $ \ (SEBS evt cx) -> SEBS evt $ Map.alter (addListener v) (SEBId s (cellId c)) cx
    execListener s v act
  where
    addListener v Nothing = pure $ SEBC v (Map.singleton i act)
    addListener _ (Just (SEBC v ls)) = pure $ SEBC v $ Map.insert i (fromJust $ cast act) ls
handleEvent (CancelEvt (Cancel _)) = pure ()
handleEvent (ForkEvt (Fork s i act)) = do
    lift $ runReaderT (runEventT $ act (inScope s)) (appendScope i s)

inScope :: Scope -> (forall a. SEB a -> SEB a)
inScope s = EventT . local (const s) . runEventT

execListener :: Value a => Scope -> a -> (a -> SEB ()) -> SEB ()
execListener s a m = inScope s (m a)

maybeMerge :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeMerge f a b = liftA2 f a b <|> a <|> b

searchCell :: forall a. Value a => Scope -> Cell SEB a -> Map SEBId SEBCell -> Maybe (a, [a -> SEB ()])
searchCell s i cx = maybeMerge mergeCell fromThisScope fromParentScope
  where
    mergeCell (a, lsA) (b, lsB) = (a /\ b, lsA ++ lsB)
    fromThisScope = do
        SEBC a lsA <- Map.lookup (SEBId s $ cellId i) cx
        liftA2 (,) (cast a) (cast $ Map.elems lsA)
    fromParentScope = do 
        s' <- parentScope s
        searchCell s' i cx

instance MonadEvent (Evt SimpleEventBus) SimpleEventBus where
    fire e = SEB $ modify (\(SEBS evts cx) -> SEBS (Set.insert e evts) cx)

instance MonadRef SimpleEventBus where
    getVal s i = SEB . gets $ orError . fmap fst . searchCell s (EventCell i) . cells
      where
        orError = fromMaybe (error $ "cell " ++ show i ++ " not found in scope " ++ show s)
