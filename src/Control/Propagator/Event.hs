{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE ApplicativeDo        #-}
module Control.Propagator.Event where

import "base" GHC.Generics
import "base" Data.Typeable
import "base" Data.Maybe
import "base" Data.Foldable
import "base" Data.List.NonEmpty
import "base" Data.Functor.Classes
import "base" Data.Type.Equality
import "base" Control.Applicative
import "base" Control.Monad
import "base" Unsafe.Coerce
import "base" Debug.Trace

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

import "transformers" Control.Monad.Trans.Reader ( ReaderT(..) )
import "transformers" Control.Monad.Trans.State ( StateT(..), evalStateT )
import "transformers" Control.Monad.Trans.Class

import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.State.Class

import "deepseq" Control.DeepSeq

import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Id


data Event m where
    Create :: Value a => Scope -> Id -> a -> Event m
    Write :: Value a => Scope -> Cell m a -> a -> Event m
    Watch :: Value a => Scope -> Cell m a -> Id -> (a -> m ()) -> Event m
    Fork :: Scope -> Id -> (LiftParent m -> m ()) -> Event m
    Cancel :: Subscription m -> Event m

instance PropagatorMonad m => Show (Event m) where
    showsPrec d (Create s i a)
        = showParen (d >= 10)
        $ showString "Create "
        . shows s
        . showString " "
        . shows i
        . showString " "
        . shows a
    showsPrec d (Write s i a)
        = showParen (d >= 10)
        $ showString "Write "
        . shows s
        . showString " "
        . shows i
        . showString " "
        . shows a
    showsPrec d (Watch s c i _)
        = showParen (d >= 10)
        $ showString "Watch "
        . shows s
        . showString " "
        . shows c
        . showString " "
        . shows i
    showsPrec d (Fork s i _)
        = showParen (d >= 10)
        $ showString "Fork "
        . shows s
        . showString " "
        . shows i
    showsPrec d (Cancel s)
        = showParen (d >= 10)
        $ showString "Cancel "
        . shows s

viaType :: (c a, c TypeRep, Typeable a, Typeable b)
        => Proxy c -> (forall x. c x => x -> x -> y)
        -> a -> b -> y
viaType _ f a b = case cast b of
    Just a' -> f a a'
    Nothing -> typeOf a `f` typeOf b

instance Ord (Event m) => Eq (Event m) where
    a == b = compare a b == EQ
instance (Ord1 (Cell m), Ord (Subscription m)) => Ord (Event m) where
    Create sa ia a `compare` Create sb ib b 
        = compare sa sb
        <> compare ia ib
        <> viaType (Proxy @Ord) compare a b
    Write sa ca a `compare` Write sb cb b
        = compare sa sb
        <> liftCompare undefined ca cb
        <> viaType (Proxy @Ord) compare a b
    Watch sa ca ia _ `compare` Watch sb cb ib _
        = compare sa sb
        <> liftCompare undefined ca cb
        <> compare ia ib
    Fork sa csa _ `compare` Fork sb csb _
        = compare sa sb
        <> compare csa csb
    Cancel a `compare` Cancel b = compare a b
    Create _ _ _ `compare` _ = GT
    _ `compare` Create _ _ _ = LT
    Write _ _ _ `compare` _ = GT
    Watch _ _ _ _ `compare` _ = GT
    _ `compare` Watch _ _ _ _ = LT
    _ `compare` Write _ _ _ = LT
    Fork _ _ _ `compare` _ = GT
    _ `compare` Fork _ _ _ = LT

type Evt m = Event (EventT m)

class MonadEvent e m | m -> e where
    fire :: e -> m ()

class MonadRef m where
    getVal :: Value a => Scope -> Id -> m a

newtype EventT m a = EventT
    { runEventT :: ReaderT Scope m a
    }
  deriving newtype (Functor, Applicative, Monad, MonadFail)

instance MonadTrans EventT where
    lift = EventT . lift

instance MonadId m => MonadId (EventT m) where
    newId = EventT . ReaderT . const . newId

instance TestEquality (Cell (EventT m)) where
    EventCell a `testEquality` EventCell b
        = if a == b
            then Just $ unsafeCoerce Refl
            else Nothing

instance Eq1 (Cell (EventT m)) where
    liftEq _ (cellId -> a) (cellId -> b) = a == b
instance Ord1 (Cell (EventT m)) where
    liftCompare _ (cellId -> a) (cellId -> b) = a `compare` b

instance NFData (Cell (EventT m) a)

instance Eq (Subscription (EventT m)) where
    a == b = compare a b == EQ
instance Ord (Subscription (EventT m)) where
    Sub ca ia sa `compare` Sub cb ib sb
        = compare (cellId ca) (cellId cb)
        <> compare ia ib
        <> compare sa sb
deriving instance (forall a. Show (Cell (EventT m) a)) => Show (Subscription (EventT m))

instance NFData (Subscription (EventT m)) where
    rnf (Sub c i s) = c `deepseq` i `deepseq` s `deepseq` ()

instance ( Typeable m
         , MonadId m
         , MonadRef m
         , MonadEvent (Evt m) m
         , Monad m
         ) => PropagatorMonad (EventT m) where

    newtype Cell (EventT m) a = EventCell
        { cellId :: Id
        }
      deriving newtype Show
      deriving stock (Eq, Ord, Generic)

    data Subscription (EventT m) where
        Sub :: Cell (EventT m) a -> Id -> Scope -> Subscription (EventT m)

    newCell i a = do
        i' <- newId i
        s <- EventT ask
        lift . fire $ Create s i' a
        pure . EventCell $ i'

    namedWatch c i a = do
        i' <- newId i
        s <- EventT ask
        lift . fire $ Watch s c i' a
        pure . Subscriptions . pure $ Sub c i' s

    write c a = do
        s <- EventT ask
        lift . fire $ Write s c a

    readCell (cellId -> i) = lift . flip getVal i =<< EventT ask

    cancel = mapM_ (lift . fire . Cancel) . getSubscriptions

newtype Scope = Scope (NonEmpty Id)
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Semigroup, NFData)
instance Show Scope where
    show (Scope ids) = fold . intersperse "/" . fmap show $ ids

popScope :: Scope -> Maybe Scope
popScope (Scope p) = fmap Scope . snd . uncons $ p

instance (Monad m, MonadId m, MonadEvent (Evt m) m) => Forkable (EventT m) where
    namedFork n m = do
        cur <- EventT ask
        child <- newId n
        lift . fire $ Fork cur child m

data SEBId = SEBId Scope Id
  deriving (Eq, Ord, Show, Generic)

data SEBCell where
    SEBC :: Value a => a -> Map Id (a -> SEB ()) -> SEBCell

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
    (SEBS evts cx) <- lift $ SEB get
    unless (Set.null evts) $ do
        lift . SEB . put $ SEBS Set.empty cx
        forM_ (Set.toDescList evts) $ \ evt -> do
            traceM $ show evt
            handleEvent evt
        flushSEB
    
handleEvent :: Evt SimpleEventBus -> SEB ()
handleEvent (Create s i a) = lift $ do
    SEB . modify $ \ (SEBS evt cx) -> SEBS evt $ Map.insert (SEBId s i) (SEBC a Map.empty) cx
handleEvent (Write s i a) = do
    Just (old, ls) <- searchCell s i . cells <$> lift (SEB get) 
    let a' =  old /\ a
    lift . SEB . modify $ \ (SEBS evt cx) -> SEBS evt $ Map.insert (SEBId s $ cellId i) (SEBC a' Map.empty) cx
    mapM_ ($ a') ls
handleEvent (Watch s c i a) = do
    Just (v, _) <- searchCell s c . cells <$> lift (SEB get)
    lift . SEB . modify $ \ (SEBS evt cx) -> SEBS evt $ Map.adjust addListener (SEBId s i) cx
    a v
  where
    addListener (SEBC v ls) = SEBC v $ Map.insert i (fromJust $ cast a) ls
handleEvent (Cancel _) = pure ()
handleEvent (Fork parent@(Scope s) i act) = do
    lift $ runReaderT (runEventT $ act lft) (Scope (i <| s))
  where
    lft :: forall a. SEB a -> SEB a
    lft = EventT . local (const parent) . runEventT

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
        s' <- popScope s
        searchCell s' i cx

instance MonadEvent (Evt SimpleEventBus) SimpleEventBus where
    fire e = SEB $ modify (\(SEBS evts cx) -> SEBS (Set.insert e evts) cx)

instance MonadRef SimpleEventBus where
    getVal s i = SEB . gets $ orError . fmap fst . searchCell s (EventCell i) . cells
      where
        orError = fromMaybe (error $ "cell " ++ show i ++ " not found in scope " ++ show s)
