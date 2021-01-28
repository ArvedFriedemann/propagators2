module Tests.ParseTests where

import "parsec" Text.Parsec
import "parsec" Text.Parsec.Token

import "base" Data.Either
import "base" Debug.Trace

import "this" Parsing.Parser
import "this" Data.Terms.TermFunctions
import "this" Data.Terms.Terms

parsetest1 :: IO ()
parsetest1 = do
  let exprtext = "expression lassoc 10 _[_,_]_" :: String
      tbl = fromRight (error "should not be used") $ runParser (mixfixDeclaration tpLD) () "exprtext" exprtext
      concExpr = "a [ b , a ] c [ a , b ] d" :: String
  parseTest (mixfixTermParser tpLD [tbl] (foldl apls STOP) (SCON . CUST :: String -> TermStruc String) ((SCON . CUST) <$> (lexeme tpLD $ identifier tpLD)) ) concExpr


parsetest2 :: IO ()
parsetest2 = do
  let exprtext = "expression nassoc 12 ( _ ) ;\n\
                 \expression nassoc 11 [< _ >] ;\n\
                 \expression lassoc 10 _ _" :: String
      ettbl = runParser mixfixDexlarationsParser () "exprtext" exprtext
      --concExpr = "a b c -> d e f -> g h i" :: String
      concExpr = "a(b[<d e>]c)d" :: String
      (tbl, tp) = case ettbl of
              Right t -> t
              Left err -> error $ show err
  putStrLn exprtext
  traceM $ show tbl
  parseTest (mixfixTermParser tp tbl stdlst (SCON . CUST :: String -> TermStruc String) ((SCON . CUST) <$> (lexeme tp $ identifier tp)) ) concExpr
