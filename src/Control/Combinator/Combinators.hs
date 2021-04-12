module Control.Combinator.Combinators where

import "this" Control.Propagator.Class
import "this" Data.Lattice

import "base" Data.Typeable
import "base" Control.Monad
import "base" Debug.Trace

eq :: forall m v scope a. (MonadProp m v scope, Value a, StdPtr v) => v a -> v a -> m ()
eq p1 p2 = dirEq p1 p2 >> dirEq p2 p1

data DirEq v = DirEq v
  deriving (Show, Eq, Ord)

dirEq :: (MonadProp m v scope, Value a, StdPtr v) => v a -> v a -> m ()
dirEq p1 p2 = watch' p1 (DirEq p2) (\v -> write p2 v)

eqAll :: forall m v scope a t. (MonadProp m v scope, Value a, StdPtr v, Foldable t) => t (v a) -> m ()
eqAll t = maybe (pure ()) void $ foldr eqAll' Nothing t
  where
    eqAll' i Nothing  = Just (pure i)
    eqAll' i (Just j) = Just (j >>= eq @_ @v i >> pure i)

data PropBot = PropBot
  deriving (Show, Eq, Ord)

propBot :: (MonadProp m v scope, Value a, Value b, HasBot a, HasBot b) =>
  v a -> v b -> m ()
propBot p1 p2 = watch' p1 PropBot (\v -> when (isBot v) $ write p2 bot)

data PromoteTo v = PromoteTo v
  deriving (Show, Eq, Ord)

push :: (MonadProp m v scope, Value a, StdPtr v) => v a -> v a -> m ()
push v1 v2 = do
  --v1' <- currScopePtr v1
  watch' v1 (PromoteTo v2) (\c -> parScoped (do
    --v2' <- currScopePtr v2
    --traceM $ "pushing "++show v1'++" ("++show v1++")"++" to "++show v2'++" ("++show v2++")"
    write v2 c))

promote :: (MonadProp m v scope, Value a, StdPtr v) => v a -> m ()
promote v = push v v





--
