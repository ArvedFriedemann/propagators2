module Tests.ParseTests where

import "parsec" Text.Parsec
import "parsec" Text.Parsec.Token

import "base" Data.Either

import "this" Parsing.Parser
import "this" Data.Terms.TermFunctions
import "this" Data.Terms.Terms

parsetest1 :: IO ()
parsetest1 = do
  let exprtext = "expression lassoc 10 _[_,_]_" :: String
      tbl = fromRight (error "should not be used") $ runParser (mixfixDeclaration tpLD) () "exprtext" exprtext
      concExpr = "a [ b , a ] c [ a , b ] d" :: String
  parseTest (mixfixTermParser tpLD [tbl] (foldl apls STOP) (\s -> (SCON . CUST :: String -> TermStruc String) <$> symbol tpLD s) ((SCON . CUST) <$> (lexeme tpLD $ identifier tpLD)) ) concExpr
