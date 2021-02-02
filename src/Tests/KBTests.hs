module Tests.KBTests where

import "this" Tests.TestLogic
import "this" Data.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Language.LogLang
import "this" Control.Language.Pipeline
import "this" Control.Propagator
import "this" Control.Propagator.Event
import "this" Parsing.Parser

import "base" Debug.Trace

import "parsec" Text.Parsec


data Cell = Sv Int | A | B | C | D | G | H | I | J | K | X | Y | Z deriving (Eq, Ord, Show)
instance Identifier Cell ()
instance Identifier (Cell, Cell) ()

kbtest0 :: IO ()
kbtest0 = runTestSEB @(TermId) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  --var (DIRECT $ Sv 1)
  --goal <- fromVarsAsCells (DIRECT $ Sv 0) ["A", "C"]
  goal <- fromVarsAsCells (DIRECT $ Sv 0) ["B"]
  kb <- pure $ KB {axioms = [(["A"],[a])], splittable = []}
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  {-recursiveCall (C,C) $-}
  simpleKBNetwork' 3 K kb goal
  return [goal]

kbtest0' :: IO ()
kbtest0' = runTestSEB @(TermId) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  goal <- fromVarsAsCells (DIRECT $ Sv 0) []
  kb <- pure $KB{axioms = [([],[a,a])], splittable = []}
  --this gives a solution because even though the proof failed, it is certain that the a needs to be unified.
  simpleKBNetwork' 1 K kb goal
  return [goal]

kbtest1 :: IO ()
kbtest1 = runTestSEB @(TermId) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  b <- fromVarsAsCells (DIRECT B) ["B"]
  c <- fromVarsAsCells (DIRECT C) ["C"]
  goal <- fromVarsAsCells (DIRECT $ Sv 0) []
  eq goal b
  kb <- pure $ KB {axioms = [([],[a]),([],[a,b]),([],[c,b])], splittable = []}
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  {-recursiveCall (C,C) $-}
  simpleKBNetwork' 3 K kb goal
  return [goal]

kbtest1' :: IO ()
kbtest1' = runTestSEB @(TermId) $ do
  a <- fromVarsAsCells (DIRECT A) ["A"]
  b <- fromVarsAsCells (DIRECT B) ["B"]
  --c <- fromVarsAsCells (DIRECT C) ["C"]
  goal <- fromVarsAsCells (DIRECT $ Sv 0) ["B"]
  kb <- pure $ KB {axioms = [([],[a,b]){-,([],[a])-}], splittable = []}
  --TODO: weird that this recursive call is needed. Apparently, variables cannot be read before they are created, but for the first step of this, a needs to be read.
  {-recursiveCall (C,C) $-}
  simpleKBNetwork' 2 K kb goal
  return [a,b,{-c,-}goal]

kbtest2 :: IO ()
kbtest2 = runTestSEB @(TermId) $ do
  --a <- fromVarsAsCells (DIRECT A) ["A","A"]
  --x <- fromVarsAsCells (DIRECT X) ["X","A"]
  --b <- fromVarsAsCells (DIRECT B) ["X","B"]
  --goal <- fromVarsAsCells (DIRECT G) [var (DIRECT $ Sv 1), "B"]
  --a <- fromVarsAsCells (DIRECT A) ["A", "A"]
  --x <- fromVarsAsCells (DIRECT X) ["X", "A"]
  --b <- fromVarsAsCells (DIRECT B) ["B", "X"]
  --goal <- fromVarsAsCells (DIRECT G) ["B",var (DIRECT $ Sv 1)]
  a <- fromVarsAsCells (DIRECT A) ["A", "A"]
  x <- fromVarsAsCells (DIRECT ("x" :: String)) ["X", "A"]
  --x <- fromVarsAsCells (DIRECT ("x" :: String)) ["A", "X"]
  b <- fromVarsAsCells (DIRECT B) ["B", "X"]
  goal <- fromVarsAsCells (DIRECT G) ["B",var (DIRECT $ Sv 1)]
  kb <- pure $ KB {axioms = [([],[a]),(["X"],[x,b])], splittable = []}

  --(splitClause -> Just ([pre1],post1)) <- refreshClause ("refresh1" :: String) --(["X"],[x,b])
  --(splitClause -> Just ([pre2],post2)) <- refreshClause ("refresh2" :: String) --(["X"],[x',b])
  --(splitClause -> Just ([],pre1')) <- refreshClause ("refresh1'" :: String) --(["X"],[x])
  --(splitClause -> Just ([],pre2')) <- refreshClause ("refresh2'" :: String) --(["X"],[x'])

  simpleKBNetwork' 3 K kb goal
  return $ [{-pre1,post1,pre2,post2,pre1',pre2',-}goal]

kbtest2' :: IO ()
kbtest2' = runTestSEB @(TermId) $ do
  --a <- fromVarsAsCells (DIRECT A) ["A", "C"]
  --x <- fromVarsAsCells (DIRECT X) ["A", "X"]
  --b <- fromVarsAsCells (DIRECT B) ["X", "B"]
  --goal <- fromVarsAsCells (DIRECT G) [var (DIRECT $ Sv 1), "B"]
  a <- fromVarsAsCells (DIRECT A) ["A", "A"]
  x <- fromVarsAsCells (DIRECT X) ["X", "A"]
  b <- fromVarsAsCells (DIRECT B) ["B", "X"]
  goal <- fromVarsAsCells (DIRECT G) ["B",var (DIRECT $ Sv 1)]
  (splitClause -> Just ([pre],post)) <- refreshClause ("refresh" :: String) (["X"],[x,b])
  scoped () $ const $ do
    eq goal post
    promoteTerm goal
    --watchTermRec pre
    --watchTermRec post
    --watchTermRec goal
    scoped () $ const $ do
      eq a pre
      promoteTerm pre
      watchTermRec pre
      watchTermRec a

  return $ [goal, post, pre]

kbtest3 :: IO ()
kbtest3 = runTestSEB @(TermId) $ do
  let exprtext = "expression nassoc 7 ( _ ) ;\n\
                 \expression rassoc 10 _ -> _ ;\n\
                 \expression lassoc 9 _ _ ;\n\
                 \expression nassoc 8 A ;\n\
                 \expression nassoc 8 B ;\n\
                 \a -> B ;\n\
                 \A -> b ;\n\
                 \" :: String
  let eres = runParser (fst <$> parseKB stdlst (SCON . CUST :: String -> TermStruc String) (SVAR :: String -> TermStruc String)) () "exprtext" exprtext
  case eres  of
    Left err -> error $ show err
    Right [t1,t2] -> do
      t1t <- fromVarsAsCells (DIRECT ("t1" :: String)) (fmap DIRECT t1)
      t2t <- fromVarsAsCells (DIRECT ("t2" :: String)) (fmap DIRECT t2)
      eq t1t t2t
      return [t1t,t2t]
    Right _ -> error "wrong test"

kbtest4 :: IO ()
kbtest4 = runTestSEB @(TermId) $ do
  let exprtext = "expression nassoc 7 ( _ ) ;\n\
                 \expression rassoc 10 _ -> _ ;\n\
                 \expression lassoc 9 _ _ ;\n\
                 \expression nassoc 8 A ;\n\
                 \expression nassoc 8 B ;\n\
                 \(a B) (A A) ;\n\
                 \(a B) (A a) ;\n\
                 \" :: String
  parseAndPerformProofSearch 1 () exprtext

kbtest5 :: IO ()
kbtest5 = runTestSEB @(TermId) $ do
  let exprtext = "expression nassoc 7 ( _ ) ;\n\
                 \expression rassoc 10 _ -> _ ;\n\
                 \expression lassoc 9 _ _ ;\n\
                 \expression nassoc 8 A ;\n\
                 \expression nassoc 8 B ;\n\
                 \A A ;\n\
                 \A a  -> B a ;\n\
                 \B a ;\n\
                 \" :: String
  parseAndPerformProofSearch 2 () exprtext


kbtest6 :: IO ()
kbtest6 = runTestSEB @(TermId) $ do
  let exprtext = "expression nassoc 7 ( _ ) ;\n\
                 \expression rassoc 11 _ -> _ ;\n\
                 \expression rassoc 10 _ : _ ;\n\
                 \expression lassoc 9 _ _ ;\n\
                 \expression nassoc 8 [] ;\n\
                 \expression nassoc 8 concat ;\n\
                 \expression nassoc 8 A ;\n\
                 \concat [] y y ;\n\
                 \concat xs y zs -> concat (x : xs) y (x : zs) ;\n\
                 \concat (A : A : []) (A : []) y ;\n\
                 \" :: String
  parseAndPerformProofSearch (-1) () exprtext


kbtest7 :: IO ()
kbtest7 = runTestSEB @(TermId) $ do
  let exprtext = "expression nassoc 7 ( _ ) ;\n\
                 \expression rassoc 13 _ -> _ ;\n\
                 \expression rassoc 11 _ = _ ;\n\
                 \expression rassoc 10 _ : _ ;\n\
                 \expression lassoc 9 _ _ ;\n\
                 \expression nassoc 8 A ;\n\
                 \expression nassoc 8 B ;\n\
                 \expression nassoc 8 K ;\n\
                 \x = x ;\n\
                 \A = B ;\n\
                 \K ;\n\
                 \" :: String
  parseAndPerformProofSearch (-1) () exprtext





--
