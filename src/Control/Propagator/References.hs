{-# LANGUAGE NoImplicitPrelude #-}
module Control.Propagator.References where

import "base" Prelude hiding ( read )
import "base" Control.Applicative hiding (some)
import "base" Control.Monad
import "base" Data.Maybe
import "base" Data.Typeable
import "base" Debug.Trace
import "base" GHC.Stack

import "mtl" Control.Monad.Reader

import "this" Control.MonadVar.MonadVar (MonadNew, MonadMutate, MonadRead)
import qualified "this" Control.MonadVar.MonadVar as MV

import "this" Control.Propagator.Class
import "this" Data.Lattice
import "this" Data.Some
import "this" Data.Typed
import "this" Data.Util

import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set



data SEBId where
  --TODO WARNING: Warning, typesafety is now only from the outside!
    SEBId :: {-(Identifier i a,-}( Std i) => i -> SEBId
instance Eq SEBId where
    SEBId i == SEBId j = i =~= j
instance Ord SEBId where
    SEBId i `compare` SEBId j = compareTyped i j

type SEBIdMap = Map SEBId (Some Something)
--TODO: scopes should be some list structure. In a cell, you can go up only one scope. When reading from a distant scope, you have to go down the stack until the orig and then successively punp the value up.
data TreeCell m' (v :: * -> *) (a :: *) = TreeCell {
  origScope :: [Scope v]
, parent :: Maybe (CellPtr m' v a) --TODO: cannot be created, needs to be existing reference
, value :: v a
, relnames :: v SEBIdMap
, scopenames :: v (Map (Scope v) (CellPtr m' v a))
}

data CellPtr m' (v :: * -> *) (a :: *) = CP (v (TreeCell m' v a))
deriving instance (forall b. Show (v b)) => Show (CellPtr m' v a)
deriving instance (forall b. Eq (v b)) => Eq (CellPtr m' v a)
deriving instance (forall b. Ord (v b)) => Ord (CellPtr m' v a)

unpkCP :: CellPtr m' v a -> v (TreeCell m' v a)
unpkCP (CP ptr) = ptr

readCP :: (MonadRead m v) => CellPtr m' v a -> m (TreeCell m' v a)
readCP ptr = MV.read $ unpkCP ptr

readSelector :: forall m' m v a b. (MonadRead m v) => (TreeCell m' v a -> b) -> CellPtr m' v a -> m b
readSelector sel ptr = sel <$> readCP ptr

readValue :: (MonadRead m v) => CellPtr m' v a -> m a
readValue ptr = readCP ptr >>= MV.read . value

createValCell :: (MonadNew m v) => [Scope v] -> a -> m (TreeCell m' v a)
createValCell s v = do
  val <- MV.new v
  reln <- MV.new Map.empty
  snms <- MV.new Map.empty
  return $ TreeCell{parent = Nothing, origScope = s, value = val, relnames = reln, scopenames = snms}

createValCellPar :: (MonadNew m v) => [Scope v] -> CellPtr m' v a -> a -> m (TreeCell m' v a)
createValCellPar s par v = do
  val <- MV.new v
  reln <- MV.new Map.empty
  snms <- MV.new Map.empty
  return $ TreeCell{parent = Just par, origScope = s, value = val, relnames = reln, scopenames = snms}

createTopCell :: (MonadNew m v, Value a) => [Scope v] -> m (TreeCell m' v a)
createTopCell s = createValCell s top

createTopCellPar :: (MonadNew m v, Value a) => [Scope v] -> CellPtr m' v a -> m (TreeCell m' v a)
createTopCellPar s par = createValCellPar s par top

createTopCellPtr :: forall m' m v a. (MonadNew m v, Value a) => [Scope v] -> m (CellPtr m' v a)
createTopCellPtr s = CP <$> (createTopCell s >>= MV.new)

createTopCellParPtr :: forall m' m v a. (MonadNew m v, Value a) => [Scope v] -> CellPtr m' v a -> m (CellPtr m' v a)
createTopCellParPtr s par = CP <$> (createTopCellPar s par >>= MV.new)

createValCellPtr :: (MonadNew m v) => [Scope v] -> a -> m (CellPtr m' v a)
createValCellPtr s val = CP <$> (createValCell s val >>= MV.new)

createValCellParPtr :: (MonadNew m v) => [Scope v] -> CellPtr m' v a -> a -> m (CellPtr m' v a)
createValCellParPtr s par val = CP <$> (createValCellPar s par val >>= MV.new)

accessLazyParent ::
  ( MonadRead m v
  , MonadAtomic v m' m
  , MonadReader (PropArgs m m' v) m
  , MonadFork m
  , Value a, StdPtr v, Typeable m', Typeable m) => (CellPtr m' v a) -> m (CellPtr m' v a)
accessLazyParent (CP p) = do
  mabPar <- readSelector parent (CP p)
  (split -> (topScp,tailScp)) <- readSelector origScope (CP p)
  case mabPar of
    Just p' -> return p'
    Nothing -> do
      (hasChanged,par) <- atomically $ do
        mabPar' <- readSelector parent (CP p)
        case mabPar' of
          Just p' -> return (False, p')
          Nothing -> do
            par <- createTopCellPtr tailScp
            MV.mutate_ p (\pc -> pc{parent=Just par})
            scpNms <- readSelector scopenames par
            MV.mutate_ scpNms (\mp -> Map.insert topScp (CP p) mp)
            return (True, par)

      when hasChanged $ do
        --traceM $ "pushing parent "++show par++" to "++show p
        watchEngineCurrPtr par (ScopeEq topScp) (readEngineCurrPtr par >>= \b -> writeEngineCurrPtr (CP p) b)
      return par

--TODO: When using a shared namespace for the scopes, there should be a mechanic where one can see the old value here. If a variable is created on a lower scope, it should be put in this place and be pumped up.
accessLazyNameMap :: (Ord i, MonadAtomic v m' m) => m (Map i b) -> m' (Map i b) -> (i -> b -> m' ()) -> m' b -> (b -> m ()) -> i -> m b
accessLazyNameMap getMap getMap' putMap con destr adr = do
  mabVal <- Map.lookup adr <$> getMap
  case mabVal of
    Just val -> return val
    Nothing -> do
      (res, hasChanged) <- atomically $ do
        mabVal2 <- Map.lookup adr <$> getMap'
        case mabVal2 of
          Just val -> return (val, False)
          Nothing -> do
            nptr <- con
            putMap adr nptr
            return (nptr, True)
      when hasChanged (destr res)
      return res

accessLazyNameMap' :: forall m' m v i b. (Std i, Typeable b, MonadAtomic v m' m) => v SEBIdMap -> m' b -> i -> m b
accessLazyNameMap' ptr con adr =
  fromJust . fromSome <$> accessLazyNameMap
    (MV.read ptr)
    (MV.read ptr)
    (\adr' nptr -> void $ MV.mutate ptr (\mp -> (Map.insert adr' nptr mp,nptr)))
    (Some <$> con)
    (const $ return ())
    (SEBId adr)

accessLazyTypeMap' :: forall m' m v a b. (Ord a, MonadAtomic v m' m) => v (Map a b) -> m' b -> a -> m b
accessLazyTypeMap' ptr con adr =
  accessLazyNameMap
    (MV.read ptr)
    (MV.read ptr)
    (\adr' nptr -> void $ MV.mutate ptr (\mp -> (Map.insert adr' nptr mp,nptr)))
    con
    (const $ return ())
    adr

accessRelName :: forall m' m v a i b. (Std i, Identifier i b, Typeable b, MonadAtomic v m' m) => CellPtr m' v a -> m' b -> i -> m b
accessRelName ptr con adr = do
  mp <- readSelector relnames ptr
  fromJust . fromSome <$> accessLazyNameMap
    (MV.read mp)
    (MV.read mp)
    (\adr' nptr -> void $ MV.mutate mp (\mp' -> (Map.insert adr' nptr mp',nptr)))
    (Some <$> con)
    (const $ return ())
    (SEBId adr)

data ScopeEq v = ScopeEq (Scope v)
deriving instance (Show (Scope v)) => Show (ScopeEq v)
deriving instance (Eq (Scope v)) => Eq (ScopeEq v)
deriving instance (Ord (Scope v)) => Ord (ScopeEq v)

accessScopeName :: forall m' m (v :: * -> *) a.
  ( Value a, Typeable v, Ord (Scope v)
  , MonadAtomic v m' m
  , MonadReader (PropArgs m m' v) m
  , MonadFork m
  , Typeable m, Typeable m', forall b. Show (v b), forall b. Ord (v b) ) => CellPtr m' v a -> Scope v -> m (CellPtr m' v a)
accessScopeName ptr scp = do
  mp <- readSelector scopenames ptr
  currscp <- readSelector origScope ptr
  accessLazyNameMap
    (MV.read mp)
    (MV.read mp)
    (\adr' nptr -> void $ MV.mutate mp (\mp' -> (Map.insert adr' nptr mp',nptr)))
    (createTopCellParPtr (scp : currscp) ptr)
    (\nptr -> do
      --traceM $ "pushing newptr "++show ptr++" to "++show nptr
      watchEngineCurrPtr ptr (ScopeEq scp) (readEngineCurrPtr ptr >>= \b -> writeEngineCurrPtr nptr b))
    scp

writeLattPtr :: (MonadMutate m v, Value a) => v a -> a -> m Bool
writeLattPtr ptr val = MV.mutate ptr (\old -> meetDiff val old)

--TODO: Just make new namespaces for each scope. It's the simplest solution for now.
data PropArgs m m' v = PropArgs {
  scopePath :: [Scope v]
, fixpointActions :: v (Map (Some Std) (PropArgs m m' v, m ()))
, fixpointSemaphore :: v Int
}

data ScopeT v = ScopeT {
  createdPointers :: v SEBIdMap
, createdScopes :: v SEBIdMap
}

newtype Scope (v :: * -> *) = SP (v (ScopeT v))
deriving instance (forall a. Show (v a)) => Show (Scope v)
deriving instance (forall a. Eq (v a)) => Eq (Scope v)
deriving instance (forall a. Ord (v a)) => Ord (Scope v)

unpkSP :: Scope v -> v (ScopeT v)
unpkSP (SP x) = x

instance (Dep m v
        , MonadFork m
        , MonadReader (PropArgs m m' v) m
        , MonadVar m v
        , MonadVar m' v
        , MonadAtomic v m' m
        --, forall a. (Typeable a) => Std (v a)
        , forall a. Eq (v a)
        , forall a. Ord (v a)
        , forall a. Show (v a)
        , Typeable m
        , Typeable m'
        , Typeable v) => MonadProp (m :: * -> *) (CellPtr (m' :: * -> *) v) (Scope v) where

  read :: (Value a) => CellPtr m' v a -> m a
  read ptr = readEngine ptr

  write :: (Value a) => CellPtr m' v a -> a -> m ()
  write ptr val = writeEngine ptr val

  watch :: (Value a, Std n) => CellPtr m' v a -> n -> m () -> m ()
  watch ptr name act = watchEngine ptr name act

  new :: (Identifier n a, Value a, Std n) => n -> m (CellPtr m' v a)
  new name = do
    sp <- reader scopePath
    when (null sp) $ error "Cannot create new variables below root!"
    s <- MV.read (unpkSP (head sp))
    accessLazyNameMap' (createdPointers s) (createTopCellPtr @m' sp) name

  newRelative :: (Identifier n a, Value b, Value a, Std n) => CellPtr m' v b -> n -> m (CellPtr m' v a)
  newRelative ptr name = do
    ptr' <- getScopeRef ptr
    s <- reader scopePath
    rn <- readSelector relnames ptr'
    accessLazyNameMap' rn (createTopCellPtr @m' s) name

  currScopePtr :: (Value a) => CellPtr m' v a -> m (CellPtr m' v a)
  currScopePtr = getScopeRef

  newScope :: (Std n) => n -> m (Scope v)
  newScope name = do
    s <- scope
    mp <- createdScopes <$> (MV.read (unpkSP s))
    res <- accessLazyNameMap' mp (do
      pts <- MV.new Map.empty
      sps <- MV.new Map.empty
      sp <- MV.new $ ScopeT{createdPointers = pts, createdScopes = sps}
      --traceM $ "created new scope map " ++ show sp
      return $ SP sp) name
    --traceM $ "created scope "++show res
    return res

  scoped :: (Scope v) -> m () -> m ()
  scoped sp@(SP sp') m = do
    nscp <- MV.read sp'
    --traceM $ "moving to scope "++show sp++" with pointers "++show (createdPointers nscp)
    local (\p -> p{scopePath = sp : (scopePath p)}) m

  parScoped :: HasCallStack => m () -> m ()
  parScoped = unsafeParScoped

  watchFixpoint :: (Std n) => n -> m () -> m ()
  watchFixpoint name act = do
    state <- ask
    fixP <- reader fixpointActions
    void $ accessLazyTypeMap' fixP (return (state, act)) (Some name)



readEngine :: forall (m' :: * -> *) m v a.
  ( MonadAtomic v m' m
  , MonadReader (PropArgs m m' v) m
  , MonadFork m
  , Typeable m, Typeable m', Typeable v, forall b. Show (v b), forall b. Ord (v b), Value a) => CellPtr m' v a -> m a
readEngine ptr = getScopeRef ptr >>= readEngineCurrPtr

readEngineCurrPtr :: forall (m' :: * -> *) m v a.
  ( MonadAtomic v m' m
  , MonadReader (PropArgs m m' v) m
  , MonadFork m
  , Typeable m, Typeable m', Typeable v, forall b. Show (v b), forall b. Ord (v b), Value a) => CellPtr m' v a -> m a
readEngineCurrPtr ptr = readCP ptr >>= MV.read . value

writeEngine :: forall (m' :: * -> *) m v a. (Monad m, MonadAtomic v m' m, MonadReader (PropArgs m m' v) m, Typeable v, forall b. Show (v b), forall b. Ord (v b), MonadFork m, Typeable m', Typeable m, Value a, HasCallStack) => CellPtr m' v a -> a -> m ()
writeEngine ptr val = do
  ptr' <- getScopeRef ptr
  writeEngineCurrPtr ptr' val

writeEngineCurrPtr :: forall (m' :: * -> *) m v a. (Monad m, MonadAtomic v m' m, MonadReader (PropArgs m m' v) m, Typeable v, forall b. Show (v b), forall b. Ord (v b), MonadFork m, Typeable m', Typeable m, Value a, HasCallStack) => CellPtr m' v a -> a -> m ()
writeEngineCurrPtr ptr val = do
  --unless (isTop val) $ traceM $ "writing "++show val++" into " ++ show ptr
  cp <- readCP ptr
  old <- MV.read (value cp)
  hasChanged <- writeLattPtr (value cp) val
  if hasChanged
  then notify ptr
  else return ()

watchEngine :: forall (m' :: * -> *) m v a n. (Typeable m', Typeable m, Typeable v, MonadAtomic v m' m, MonadReader (PropArgs m m' v) m, MonadFork m, Value a, Std n, StdPtr v) => CellPtr m' v a -> n -> m () -> m ()
watchEngine ptr name act = do
  ptr' <- getScopeRef ptr
  watchEngineCurrPtr ptr' name act

watchEngineCurrPtr :: forall (m' :: * -> *) m v a n. (Typeable m', Typeable m, Typeable v, MonadAtomic v m' m, MonadReader (PropArgs m m' v) m, MonadFork m, Value a, Std n, StdPtr v) => CellPtr m' v a -> n -> m () -> m ()
watchEngineCurrPtr ptr name act = do
  --traceM $ "watching "++show ptr++" with " ++ show name
  s <- readSelector origScope ptr
  propset <- getPropset @m' s ptr >>= readSelector @m' value
  hasChanged <- MV.mutate propset (\ps -> let (mabChan,mp) = Map.insertLookupWithKey (\k n o -> n) (Some name) act ps in (mp, not $ isJust mabChan))
  when hasChanged act

getScopeRef :: forall (m' :: * -> *) (m :: * -> *) v a.
  ( Monad m
  , MonadRead m v
  , MonadScope m v
  , MonadAtomic v m' m
  , MonadReader (PropArgs m m' v) m
  , MonadFork m
  , MonadUnsafeParScoped m
  , HasCallStack
  , Typeable m, Typeable m', forall b. Eq (v b), forall b. Ord (v b), forall b. Show (v b), Typeable v, Value a) => CellPtr m' v a -> m (CellPtr m' v a)
getScopeRef ptr = do
  tc <- readCP ptr
  s <- reader scopePath
  when (null s) $ error $ "Current scope below root when getting ref "++show ptr
  when (null (origScope tc)) $ error $ "Pointer scope below root when getting ref "++show ptr
  if (head $ origScope tc) == (head s)
  then return ptr
  else do
    let (reverse -> up,down,c) = longestCommonTail s (origScope tc)
    when (null c) $ error "longest common tail is empty!"
    res <- navigateScopePtr down up ptr
    {-}
    traceM $ "ScopePathRetrieval from "++show ptr++" to "++show res++
              "\norig:   "++show (origScope tc)++
              "\nptrScp: "++show s++
              "\ndeduced path: "++show (down,up,c)
              -}
    tc' <- readCP res
    --unless (origScope tc' == s) $ error "Scope of scope-pointer is not the current scope!"
    return res
  where
    navigateScopePtr :: [Scope v] -> [Scope v] -> (CellPtr m' v a) -> m (CellPtr m' v a)
    navigateScopePtr (_:s') up ptr' = accessLazyParent ptr' >>= (\p -> {-trace ("parent of "++show ptr++" is "++show p) $-} navigateScopePtr s' up p)
    navigateScopePtr [] (s:s') ptr' = accessScopeName ptr' s >>=(\p ->  {-trace ("child of "++show ptr++" is "++show p) $-} navigateScopePtr [] s' p)
    navigateScopePtr [] [] ptr' = return ptr'



class MonadUnsafeParScoped m where
  --Unsafe because this parScoped can return values and therefore potentially more pointers from forks to the orig, which should be avoided at this stage
  unsafeParScoped :: m a -> m a

instance (MonadReader (PropArgs m m' v) m) => MonadUnsafeParScoped m where
  unsafeParScoped :: (HasCallStack) => m a -> m a
  unsafeParScoped m = do
    s <- reader scopePath
    case s of
      [] -> error "Calling parScoped on empty scope!"
      [x] -> m --error "Calling parScoped on root!"
      _  -> local (\p -> p{scopePath = tail (scopePath p)}) m

class MonadScope m v where
  scope :: m (Scope v)

instance (MonadReader (PropArgs m m' v) m) => MonadScope m v where
  scope :: m (Scope v)
  scope = do
    sp <- reader scopePath
    case sp of
      [] -> error "Retrieving scope from below root!"
      _ -> return $ head sp

data PropOf m' m v = PropOf
  deriving (Show, Eq, Ord, Typeable)

type PropSetPtr m' m v = CellPtr m' v (Map (Some Std) (m ()))
instance Identifier (PropOf m' m v) (PropSetPtr m' m v)

notify :: forall (m' :: * -> *) m v a.
  ( MonadAtomic v m' m
  , MonadFork m
  , MonadScope m v
  , MonadReader (PropArgs m m' v) m
  , forall b. Show (v b)
  , Typeable m', Typeable m, Typeable v) => CellPtr m' v a -> m ()
notify ptr = do
  --traceM $ "notifying ptr "++show ptr
  s <- readSelector origScope ptr
  when (null s) $ error "origScope of pointer null in notify"
  propset <- getPropset s ptr >>= readValue
  local (\p -> p{scopePath = s}) $ forkF (Map.elems propset)

getPropset :: forall (m' :: * -> *) m v a.
  ( Typeable m', Typeable m, Typeable v
  , MonadAtomic v m' m
  , MonadScope m v
  , forall b. Show (v b)) => [Scope v] -> CellPtr m' v a -> m (PropSetPtr m' m v)
getPropset s ptr = do
  accessRelName ptr (createValCellPtr @m' s Map.empty) (PropOf @m' @m @v)

--
