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
  let exprtext = "expression rassoc 11 _ k _ k _ ;\
                 \expression lassoc 10 _ _" :: String
      ettbl = runParser (sepBy1 (mixfixDeclaration tpLD) (lexeme tpLD $ symbol tpLD ";") ) () "exprtext" exprtext
      --concExpr = "a b c -> d e f -> g h i" :: String
      concExpr = "a k b c k d" :: String
      tbl = case ettbl of
              Right t -> t
              Left err -> error $ show err
  traceM $ show tbl
  parseTest (mixfixTermParser tpLD' tbl stdlst (SCON . CUST :: String -> TermStruc String) ((SCON . CUST) <$> (lexeme tpLD' $ identifier tpLD')) ) concExpr
