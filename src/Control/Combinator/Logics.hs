{-# LANGUAGE NoImplicitPrelude #-}
module Control.Combinator.Logics where

import "base" Prelude hiding ( read )
import "base" Control.Monad
import "base" Data.Maybe
import "base" Debug.Trace

import "this" Control.Propagator.Class
import "this" Control.Combinator.Combinators
import "this" Data.Lattice
import "this" Data.Some

data ScopeIdx a = ScopeIdx (Some Std) Int
  deriving (Show, Eq, Ord)
instance Identifier (ScopeIdx a) a

class Promoter a m where
  promoteAction :: a -> m ()

disjunctFork :: (MonadProp m v scope, Value a, HasBot a, Std n, StdPtr v) => n -> v a -> [m ()] -> m ()
disjunctFork ctx succPtr ms = disjunctForkPromoterDestr ctx succPtr (write succPtr bot) [(m, promote succPtr) | m <- ms]

disjunctForkPromote :: (MonadProp m v scope, Promoter (v a) m, Value a, HasBot a, Std n, StdPtr v) => n -> v a -> [m ()] -> m ()
disjunctForkPromote ctx succPtr ms = disjunctForkPromoterDestr ctx succPtr (write succPtr bot) [(m, promoteAction succPtr) | m <- ms]

disjunctForkPromoterDestr :: (MonadProp m v scope, Value a, HasBot a, Std n, StdPtr v) => n -> v a -> m () -> [(m (), m ())] -> m ()
disjunctForkPromoterDestr ctx succPtr finDestr ms = do
  scopeVars <- forM (zip ms [0..]) (\((_,destr),i) ->
    let idx = ScopeIdx (Some ctx) i in do
      ptr <- new idx
      scp <- newScope idx
      return (scp, ptr, destr)
    )
  forM_ (zip ms scopeVars) $ \((act, _), (tmpScp, tmpPtr,_)) -> do
    scoped tmpScp $ do
      push succPtr tmpPtr
      act
  --two watched literal scheme
  case scopeVars of
    ((_,p1,_):(_,p2,_):_) -> do
      watch p1 (DetermineWinner ctx) (determineWinner finDestr scopeVars)
      watch p2 (DetermineWinner ctx) (determineWinner finDestr scopeVars)
    [(_,p,_)] -> do
      watch p (DetermineWinner ctx) (determineWinner finDestr scopeVars)
    [] -> finDestr

data DetermineWinner i = DetermineWinner i
  deriving (Show, Eq, Ord)

determineWinner :: (MonadProp m v scope, Value a, HasBot a, StdPtr v) => m () -> [(scope, v a, m ())] -> m ()
determineWinner finDestr lst = do
  succeeded <- (catMaybes <$>) $ forM lst $ \(s,p,m) -> do
    r <- read p
    if isBot r
    then return Nothing
    else return $ Just (s,p,m)
  traceM $ "Succeeded pointers: "++(show $ map (\(_,p,_) -> p) succeeded)
  case succeeded of
    [(s,_,m)] -> traceM "\n>>> Promoting winner!\n" >> scoped s m
    [] -> finDestr
    _ -> return ()





--
