module Tests.ParseTests where

import "parsec" Text.Parsec
import "parsec" Text.Parsec.Token

import "this" Parsing.Parser

parsetest1 :: IO ()
parsetest1 = do
  let exprtext = "expression test"::String-- lassoc 10 _[_,_]_" :: String
  parseTest ((lexeme tpLD $ reserved tpLD "expression") >> identifier tpLD) exprtext
  --parseTest (identifier tpLD) exprtext
  --parseTest (mixfixDeclaration tpLD) exprtext
