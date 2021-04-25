module Violet.Compiler where


import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Violet.Parser

updateName :: String -> State (Map String Int) Int
updateName name = do
  mp <- get
  case Map.lookup name mp of
    Nothing -> do
      put $ Map.insert name 1 mp
      return 0
    Just i -> do
      put $ Map.insert name (i+1) mp
      return i

getNameCount :: String -> State (Map String Int) Int
getNameCount name = do
  mp <- get
  case Map.lookup name mp of
    Nothing -> do
      put $ Map.insert name 1 mp
      return 0
    Just i -> do
      put $ Map.insert name (i+1) mp
      return i


ssaVarName :: String -> Int -> String
ssaVarName n i = '_' : n ++ (show i)


ssaExpr :: AssignExpr -> State (Map String Int) AssignExpr
ssaExpr (Var name) = do
  mp <- get
  let i = Map.findWithDefault 0 name mp
  if (i <= 0) then do error $ "Trying to assign to non initialized variable \"" ++ name ++ "\""
    else do return (Var (ssaVarName name (i-1)))
ssaExpr (Negation e) = do
  e' <- ssaExpr e
  return $ Negation e'
ssaExpr (Sum lhs rhs) = do
  lhs' <- ssaExpr lhs
  rhs' <- ssaExpr rhs
  return $ Sum lhs' rhs'
ssaExpr (Subtr lhs rhs) = do
  lhs' <- ssaExpr lhs
  rhs' <- ssaExpr rhs
  return $ Subtr lhs' rhs'
ssaExpr (Product lhs rhs) = do
  lhs' <- ssaExpr lhs
  rhs' <- ssaExpr rhs
  return $ Product lhs' rhs'
ssaExpr (Division lhs rhs) = do
  lhs' <- ssaExpr lhs
  rhs' <- ssaExpr rhs
  return $ Division lhs' rhs'
ssaExpr e = return e


ssaAssignments :: [Assign] -> State (Map String Int) [Assign]
ssaAssignments [] = return []
ssaAssignments (a@(Assign varName expr) : as) = do
  mp <- get
  let i = Map.findWithDefault 0 varName mp
  let newName = ssaVarName varName i
  expr' <- ssaExpr expr
  put $ Map.insert varName (i + 1) mp
  rest <- ssaAssignments as
  return $ (Assign newName expr') : rest

clearNames :: [Assign] -> State (Map String Int) [Assign]
clearNames [] = return []
clearNames (a@(Assign varName sth) : xs) = do
  var_numbering <- updateName varName
  let new_name = varName ++ (show var_numbering)
  rest <- clearNames xs
  return $ (Assign new_name sth) : rest

tr t s = evalState (t $ parseString s) Map.empty

arved :: String -> [Assign]
arved = tr clearNames

ssa :: String -> [Assign]
ssa = tr ssaAssignments

res = ssa "x = 4 x = 3 * x y = 0 y = x*y+4"
