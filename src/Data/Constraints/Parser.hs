module Data.Constraints.Parser
    ( constraint
    ) where

import "base" Data.Functor
import "base" Control.Applicative ( liftA2, liftA3 )

import "parsec" Text.ParserCombinators.Parsec

import "this" Data.Constraints.Types


lexeme :: CharParser st a -> CharParser st a
lexeme p = p <* spaces

symbol :: String -> CharParser st String
symbol name = lexeme (string name)

parens :: CharParser st a -> CharParser st a
parens p = between (symbol "(") (symbol ")") p

constraint :: CharParser st Constraint
constraint = liftA3 Constraint expr rel expr

rel :: CharParser st Ordering
rel = symbol ">" $> GT
  <|> symbol "=" $> EQ
  <|> symbol "<" $> LT

expr :: CharParser st Expr
expr = term `chainl1` addop

term :: CharParser st Expr
term = factor `chainl1` mulop

factor :: CharParser st Expr
factor = parens expr <|> integer <|> varExpr

mulop :: CharParser st Op
mulop = op "*" MulOp <|> op "/" DivOp

addop :: CharParser st Op
addop = op "+" AddOp <|> op "-" SubOp

type Op = Expr -> Expr -> Expr
op :: String -> BinOp -> CharParser st Op
op s o = symbol s $> BinopExpr o

integer :: CharParser st Expr
integer = IntExpr . read <$> lexeme (many1 digit)

varExpr :: CharParser st Expr
varExpr = VarExpr <$> lexeme ident
  where
    ident = liftA2 (:) small $ many idchar
    small = lower <|> char '_'
    idchar = small <|> upper <|> digit <|> char '\''
