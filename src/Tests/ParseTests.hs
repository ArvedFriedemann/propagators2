module Tests.ParseTests where

import "parsec" Text.Parsec
import "parsec" Text.Parsec.Token

import "base" Data.Either

import "this" Parsing.Parser
import "this" Data.Terms.TermFunctions

parsetest1 :: IO ()
parsetest1 = do
  let exprtext = "expression lassoc 10 _[_,_]_" :: String
      tbl = fromRight (error "should not be used") $ runParser (mixfixDeclaration tpLD) () "exprtext" exprtext
      concExpr = "a [ b , a ] c" :: String
  parseTest (mixfixTermParser [tbl] (foldr SAPPL STOP) (\s -> SVAR <$> symbol tpLD s) (SVAR <$> (lexeme tpLD $ identifier tpLD)) SAPPL) concExpr
