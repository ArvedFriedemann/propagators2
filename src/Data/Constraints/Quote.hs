{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Constraints.Quote
    ( con
    ) where

import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote

import "parsec" Text.Parsec

import "this" Data.Constraints.Types
import "this" Data.Constraints.Parser
import "this" Data.Constraints.Combinators


con :: QuasiQuoter
con = QuasiQuoter
    { quoteExp = quoteConstraintExp
    , quotePat  = error "can not create fact pattern"
    , quoteType = error "can not create fact type"
    , quoteDec  = error "can not create fact declaration"
    }

parseConstraint :: MonadFail m => (String, Int, Int) -> String -> m Constraint
parseConstraint (file, line, col) s =
    case runParser p () "" s of
      Left err -> fail $ show err
      Right e  -> return e
  where
    p = do
        pos <- getPosition
        setPosition $
            (flip setSourceName) file $
            (flip setSourceLine) line $
            (flip setSourceColumn) col $
            pos
        spaces
        e <- constraint
        eof
        return e

quoteConstraintExp :: String -> ExpQ
quoteConstraintExp s = do
    loc <- location
    let pos = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))
    cx <- parseConstraint pos s
    constraintToExpQ cx

constraintToExpQ :: Constraint -> ExpQ
constraintToExpQ (Constraint (VarExpr n) EQ (BinopExpr AddOp (VarExpr a) (VarExpr b)))
    = [| add $(var a) $(var b) $(var n) |]
constraintToExpQ (Constraint (BinopExpr AddOp (VarExpr a) (VarExpr b)) EQ (VarExpr n))
    = [| add $(var a) $(var b) $(var n) |]
constraintToExpQ _ = fail "unsupported"

var :: String -> ExpQ
var = varE . mkName
