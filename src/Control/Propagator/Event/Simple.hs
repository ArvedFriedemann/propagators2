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


data SEBId where
    SEBId :: Identifier i a => Scope -> i -> SEBId

data SEBCell where
    SEBC :: Value a => a -> Map SomeStd (a -> SEB ()) -> SEBCell
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

runSEB :: SEB a -> (a -> SEB b) -> IO b
runSEB start end = do
    flip evalStateT (SEBS Set.empty Map.empty) . unSEB . flip runReaderT Root . runEventT $ do
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
handleEvent (WriteEvt (Write s i a)) = do
    (old, ls) <- searchCell s i . cells <$> lift (SEB get) 
    let a' =  old /\ a
    unless (a' == old) $ do
        lift . SEB . modify $ \ (SEBS evt cx) -> SEBS evt $ Map.adjust (setValue a') (SEBId s i) cx
        mapM_ (execListener s a') ls
  where
    setValue a' (SEBC _ ls) = SEBC (fromJust $ cast a') ls
handleEvent (WatchEvt (Watch s c i act)) = do
    (v, _) <- searchCell s c . cells <$> lift (SEB get)
    lift . SEB . modify $ \ (SEBS evt cx) -> SEBS evt $ Map.alter (addListener v) (SEBId s c) cx
    execListener s v act
  where
    addListener v Nothing = pure $ SEBC v (Map.singleton (SomeStd i) act)
    addListener _ (Just (SEBC v ls)) = pure $ SEBC v $ Map.insert (SomeStd i) (fromJust $ cast act) ls
handleEvent (ForkEvt (Fork s i act)) = do
    lift $ runReaderT (runEventT $ act (inScope s)) (Scope i s)

inScope :: Scope -> (forall a. SEB a -> SEB a)
inScope s = EventT . local (const s) . runEventT

execListener :: Value a => Scope -> a -> (a -> SEB ()) -> SEB ()
execListener s a m = inScope s (m a)

searchCell :: forall a i. Identifier i a => Scope -> i -> Map SEBId SEBCell -> (a, [a -> SEB ()])
searchCell s i cx = maybeMerge mergeCell fromThisScope fromParentScope
  where
    mergeCell (a, lsA) (b, lsB) = (a /\ b, lsA ++ lsB)
    fromThisScope = do
        SEBC a lsA <- Map.lookup (SEBId s i) cx
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
