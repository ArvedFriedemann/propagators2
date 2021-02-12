{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE StrictData        #-}
module Control.Combinator.Logics
    ( disjunctFork
    , disjunctForkDestr
    , disjunctForkPromoter
    , Promoter
    , promoteAction
    ) where

import "base" Prelude hiding ( read )
import "base" Data.Functor
import "base" Control.Monad
import "base" Data.Typeable
import "base" GHC.Generics

import "hashable" Data.Hashable

import "this" Control.Propagator
import "this" Data.Lattice


data DisjunctFork i j = DisjunctFork
    { name :: j
    , target :: i
    , index :: {-# UNPACK #-} Int
    } deriving (Eq, Ord, Show, Generic)
instance (Hashable i, Hashable j) => Hashable (DisjunctFork i j)
instance (Std j, Identifier i a) => Identifier (DisjunctFork i j) a

disjunctFork :: forall i j a m.
             ( MonadProp m
             , Typeable m
             , BoundedJoin a, Identifier i a
             , Std j
             ) => i -> j -> [m ()] -> m ()
disjunctFork goal name ms = disjunctForkDestr goal name (zip ms (repeat $ promote goal)) (void $ write goal bot)
{-# INLINE disjunctFork #-}

class (Identifier i a) => Promoter i a m | i -> a where
  promoteAction :: i -> m ()

disjunctForkPromoter :: forall i j a m.
             ( MonadProp m
             , Typeable m
             , BoundedJoin a
             , Identifier i a
             , Promoter i a m
             , Std j
             ) => i -> j -> [m ()] -> m ()
disjunctForkPromoter goal name ms = disjunctForkDestr goal name (zip ms (repeat $ promoteAction goal )) (void $ write goal bot)
{-# INLINE disjunctForkPromoter #-}

disjunctForkDestr :: forall i j a m.
             ( MonadProp m
             , Typeable m
             , BoundedJoin a, Identifier i a
             , Std j
             ) => i -> j -> [(m (), m ())] -> m () -> m ()
disjunctForkDestr _ _ [] finDestr = finDestr
disjunctForkDestr sucvar name ms finDestr = djfs `forM_` \(djf, (constr , _)) -> do
    watch djf $ PropagateWinner djfsDestr finDestr
    scoped djf $ do
        push sucvar djf
        constr
  where
    djfs :: [(DisjunctFork i j, (m (), m ()))]
    djfs = zipWith (\n m -> (DisjunctFork name sucvar n, m) ) [0..] ms
    {-# INLINE djfs#-}
    djfsDestr :: [(DisjunctFork i j, m ())]
    djfsDestr = map (\(x,(_,z)) -> (x, z)) djfs
    {-# INLINE djfsDestr #-}
{-# INLINE disjunctForkDestr #-}

data PropagateWinner i j m = PropagateWinner [(DisjunctFork i j, m ())] (m ())
instance (Hashable i, Hashable j) => Hashable (PropagateWinner i j m) where
    hashWithSalt n (PropagateWinner a _) = hashWithSalt n (fst <$> a)
instance (Eq i, Eq j) => Eq (PropagateWinner i j m) where
    (PropagateWinner a _) == (PropagateWinner b _) = (fst <$> a) == (fst <$> b)
instance (Ord i, Ord j) => Ord (PropagateWinner i j m) where
    compare (PropagateWinner a _) (PropagateWinner b _) = compare (fst <$> a) (fst <$> b)
instance (Show i, Show j) => Show (PropagateWinner i j m) where
    show (PropagateWinner a _) = "PropagateWinner " ++ (show (fst <$> a))

instance (Std j, Typeable m, MonadProp m, Value a, BoundedJoin a, Identifier i a)
         => Propagator m a (PropagateWinner i j m) where
    propagate (PropagateWinner forks finalDestr) _ = do

        fconts <- fmap join . forM forks $ \(f,m) -> read f <&> \case
            Bot -> []
            _   -> [(f,m)]
        case fconts of
            [(f,m)] -> do
                scoped f m
            []   -> do
              finalDestr
            _ -> pure ()
