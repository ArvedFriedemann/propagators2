{-# LANGUAGE NoImplicitPrelude #-}
module Tests.UnificationTest where

import "base" Prelude hiding ( read )
import "base" GHC.Generics
import "base" Debug.Trace

import "containers" Data.Map ( Map )
import "containers" Data.Map qualified as Map

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Data.Terms
import "this" Control.Combinator.Logics
import "this" Control.Propagator
import "this" Control.Propagator.Event ( evalSEB , SEB)
import "this" Tests.TestLogic
import "this" Data.Lattice
import "this" Control.Language.LogLang


data Cell = STR String | Sv Int | A | B | C | D deriving (Eq, Ord, Show)
instance Identifier Cell (TermSet Cell)

test1 :: IO ()
test1 = runTestSEB @(TermId) $ do
    sv_a <- fromVarsAsCells (DIRECT A) $ [var (DIRECT $ Sv 0),"a"]
    b_sv <- fromVarsAsCells (DIRECT B) $ ["b", var (DIRECT $ Sv 0)]
    sv_a `eq` b_sv
    return [sv_a, b_sv, DIRECT $ Sv 0]

test2 :: IO ()
test2 = runTestSEB @(TermId) $ do
    t1 <- fromVarsAsCells (DIRECT A) [var $ DIRECT $ Sv 1, "a", var $ DIRECT $ Sv 1]
    t2 <- fromVarsAsCells (DIRECT B) $ [var (DIRECT $ Sv 2), "a"]
    t1 `eq` t2
    return [t1, t2, DIRECT $ Sv 1, DIRECT $ Sv 2]


test3 :: IO ()
test3 = runTestSEB @(TermId) $ do
    t1 <- fromVarsAsCells (DIRECT A) $ var $ DIRECT $ Sv 0
    t2 <- fromVarsAsCells (DIRECT B) $ "a" <> var (DIRECT $ Sv 0)
    t1 `eq` t2
    return [t1, t2, DIRECT $ Sv 0]

test4 :: IO ()
test4 = runTestSEB @(TermId) $ do
    scoped () $ \_ -> do
        promote (DIRECT A)
        write (DIRECT A) "A"
    return [DIRECT A]

test42 :: IO ()
test42 = runTestSEB @(TermId) $ do

    scoped () $ \_ -> do
      --read (DIRECT A)
      push (DIRECT A) (DIRECT B)
      --eq (DIRECT A) (DIRECT B)
    write (DIRECT A) "A"
    return [DIRECT A, DIRECT B]

test44' :: IO ()
test44' = runTestSEB @(TermId) $ do

    scoped () $ \_ -> do
      promote (DIRECT B)
      scoped () $ \_ -> do
        push (DIRECT A) (DIRECT B)
    write (DIRECT A) "A"
    return [DIRECT A, DIRECT B]

test43 :: IO ()
test43 = runTestSEB @(TermId) $ do

    scoped () $ \_ -> do
      --read (DIRECT A)
      push (DIRECT B) (DIRECT B)
      scoped () $ \_ -> do
        push (DIRECT A) (DIRECT B)
        --eq (DIRECT A) (DIRECT B)
    write (DIRECT A) "A"
    return [DIRECT A, DIRECT B]

test44 :: IO ()
test44 = runTestSEB @(TermId) $ do

    scoped () $ \_ -> do
      --read (DIRECT A)
      promoteTerm (DIRECT B)
      scoped () $ \_ -> do
        promoteTerm (DIRECT B)
        eq (DIRECT A) (DIRECT B)
        --eq (DIRECT A) (DIRECT B)
    fromVarsAsCells (DIRECT A) ["A","B","C"]
    return [DIRECT A, DIRECT B]

test46 :: IO ()
test46 = runTestSEB @(TermId) $ do

    scoped () $ \_ -> do
      --read (DIRECT A)
      promoteTerm (DIRECT B)
      scoped () $ \_ -> do
        --read (DIRECT A)
        promoteTerm (DIRECT B)
        scoped () $ \_ -> do
          --read (DIRECT A)
          promoteTerm (DIRECT B)
          scoped () $ \_ -> do
            promoteTerm (DIRECT B)
            eq (DIRECT A) (DIRECT B)
            --eq (DIRECT A) (DIRECT B)
    fromVarsAsCells (DIRECT A) ["A","B","C"]
    return [DIRECT A, DIRECT B]

test45 :: IO ()
test45 = runTestSEB @(TermId) $ do

    disjunctForkPromoter (DIRECT A) ("djf0" :: String) [(do
        --promoteTerm (DIRECT A)
        --eq (DIRECT A) (DIRECT B)
        fromVarsAsCells (DIRECT B) ["B", "X"]
        fromVarsAsCells (DIRECT C) ["X", "A"]
        --promoteTerm (DIRECT A)
        [pre,post] <- refreshClause ("rfc1"::String) (["X"],[(DIRECT C),(DIRECT B)])
        eq (DIRECT A) post

        disjunctForkPromoter pre ("djf1" :: String) [(do
            s <- scope
            traceM $ "Fork2: "++show s
            fromVarsAsCells (DIRECT D) ["A","A"]
            --promoteTerm (DIRECT C)
            eq (DIRECT D) pre
            pure ()
          ),(do
            [_,post'] <- refreshClause ("rfc1"::String) (["X"],[(DIRECT C),(DIRECT B)])
            eq pre post'
            pure ()
          )]

        pure ()
      ), (do
        fromVarsAsCells (DIRECT D) ["A","A"]
        eq (DIRECT A) (DIRECT D)
        pure ()
      )]
    fromVarsAsCells (DIRECT A) ["B", var (DIRECT $Sv 0)]
    return [DIRECT A]

test47 :: IO ()
test47 = runTestSEB @(TermId) $ do
    disjunctForkPromoter (DIRECT A) ("djf0" :: String) [(do
        --promoteTerm (DIRECT A)
        disjunctForkPromoter (DIRECT A) ("djf1" :: String) [(do
            --fromVarsAsCells (DIRECT A) ["A","B"]
            --promoteTerm (DIRECT A)
            disjunctForkPromoter (DIRECT A) ("djf2" :: String) [(do
                s <- scope
                traceM $ "Arrived at scope "++show s
                watch (DIRECT A) $ UniversalPropagator $ ((do
                  da <- fromCellSize 100 (DIRECT A)
                  traceM $ "DIRECT A index=0 is "++show da
                  ):: SEB ())
                --promoteTerm (DIRECT A)
                fromVarsAsCells (DIRECT A) ["A","B"]
                pure ()
              ),(do
                s <- scope
                traceM $ "Actually killing djf2 index=1 in "++show s
                watch (DIRECT A) $ UniversalPropagator $ ((do
                  da <- fromCellSize 100 (DIRECT A)
                  traceM $ "DIRECT A index=1 is "++show da
                  ):: SEB ())
                write (DIRECT A) bot
                pure ()
              )]
            pure ()
          ),(do
            write (DIRECT A) bot
            pure ()
          )]
        pure ()
      ), (do
        write (DIRECT A) bot
        pure ()
      )]
    return [DIRECT A]

test4' :: IO ()
test4' = runTestSEB @(TermId) $ do
    let [orig, t1, t2] = DIRECT <$> [A, B, C] :: [TermId]
    write t1 TSBot
    write t2 "A"
    disjunctForkPromoter orig ()
        [ do
            orig `eq` t1
        , do
            orig `eq` t2
        ]
    return [orig, t1, t2]

test43' :: IO ()
test43' = runTestSEB @(TermId) $ do
    let [orig, t1, t2] = DIRECT <$> [A, B, C] :: [TermId]
    write t1 TSBot
    write t2 "A"
    scope >>= \s -> traceM $ "\n\nscope0:"++show s++"\n\n"
    disjunctForkPromoter orig (1 :: Int)
        [ do
            scope >>= \s -> traceM $ "\n\nscope1:"++show s++"\n\n"
            orig `eq` t2
            disjunctForkPromoter t2 (2 :: Int)
              [do
                scope >>= \s -> traceM $ "\n\nscope2:"++show s++"\n\n"
                t2 `eq` t1
              ]
        ]
    return [orig, t1, t2]

test42' :: IO ()
test42' = runTestSEB @(TermId) $ do
    let [orig, t1, t2] = DIRECT <$> [A, B, C] :: [TermId]

    disjunctForkPromoter orig ()
        [ do
            write t1 TSBot
            orig `eq` t1
        , do
            write t1 TSBot
            orig `eq` t1
        ]
    return [orig, t1, t2]

test4'' :: IO ()
test4'' = runTestSEB @(TermId) $ do
    orig <- fromVarsAsCells (DIRECT A) []
    disjunctForkPromoter orig ()
        [ do
            t <- fromVarsAsCells (DIRECT B) ["A","B","C"]
            orig `eq` t
        , do
            t <- fromVarsAsCells (DIRECT B) bot
            orig `eq` t
        ]
    return [orig]

testRefreshTo :: IO ()
testRefreshTo = runTestSEB $ do
    orig <- fromVarsAsCells (DIRECT (Sv 0)) ["B","A"]
    orig' <- fromVarsAsCells (DIRECT (Sv 1)) ["B","A"]
    --so the term listeners are placed
    --v1 <- fromVarsAsCells (DIRECT C) []
    --cpy <- return $ DIRECT D
    traceM $ show $ ([(CUST ("B" :: String),bound B "B")] ::[(TermConst,TermId)])
    cpy <- refreshVarsTbl ("refresh0" :: String) (Map.fromList [(CUST ("B" :: String),bound B "B")]) orig
    cpy' <- refreshVarsTbl ("refresh1" :: String) (Map.fromList [(CUST ("B" :: String),bound C "B")]) orig

    eq orig' cpy
    eq orig' cpy'
    --cpy <- refreshVarsTbl B [("B",v1 :: TermId)] orig
    return [orig,orig', cpy, cpy']
{-
testRefreshBack :: IO ()
testRefreshBack = runTestSEB $ do
    --so the term listeners are placed
    orig <- fromVarsAsCells []
    v1 <- fromVarsAsCells []
    copy <- fromVarsAsCells [var v1, "a"]
    refreshVarsTbl [(CUSTOM "b",v1)] orig copy
    return [orig, copy]
-}

testRefreshUnification :: IO ()
testRefreshUnification = runTestSEB @(TermId) $ do
  v1 <- fromVarsAsCells (DIRECT A) []
  v2 <- fromVarsAsCells (DIRECT B) []
  orig <- fromVarsAsCells (DIRECT $ STR "orig")
    [["a", "a"], var v2]
  rule <- fromVarsAsCells (DIRECT $ STR "rule")
    [["b", "a"], "b"]
  cpy <- refreshVarsTbl (STR "copy" ) [("b",v1 :: TermId)] rule
  eq orig cpy
  return [rule, cpy, orig]


data TD = TD_A | TD_B | TD_C
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

data TC = Orig | TC Int deriving (Eq, Ord, Show)
instance Identifier TC (Domain TD)

test5 :: IO ()
test5 = flip evalSEB (>> pure ()) $ do
    write (TC 2) [TD_A, TD_C]

    error "TODO: Scoped no longer exists. Use relative fork addressing"
    --scoped () $ \s -> watch (TC 2) $ Scoped s $ Write $ TC 2

    watch Orig $ Const ([TD_A] :: Domain TD) $ Write $ TC 2

    pure $ do
        v <- read Orig
        traceM $ show v
