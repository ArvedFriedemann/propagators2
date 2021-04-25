{-# LANGUAGE OverloadedStrings #-}
module Violet.Parser where



import Control.Monad.Identity
import Control.Monad.Combinators.Expr

import Data.Bifunctor
import Data.Either
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


data Expr v c
  = Var v
  | Constant c
  | Negation (Expr v c)
  | Sum      (Expr v c) (Expr v c)
  | Subtr    (Expr v c) (Expr v c)
  | Product  (Expr v c) (Expr v c)
  | Division (Expr v c) (Expr v c)
  deriving (Eq, Ord, Show)

type AssignExpr = Expr String Int
data Assign = Assign String AssignExpr
  deriving (Eq, Ord, Show)

operatorTable :: [[Operator Parser AssignExpr]]
operatorTable =
  [ [ prefix "-" Negation ]
  , [ binary "*" Product
    , binary "/" Division
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  ]

binary :: Text -> (AssignExpr -> AssignExpr -> AssignExpr) -> Operator Parser AssignExpr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (AssignExpr -> AssignExpr) -> Operator Parser AssignExpr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)


spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1                         -- (2)
  (L.skipLineComment "#")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

type Parser = Parsec Void Text

pVariable :: Parser AssignExpr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

myVar = lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser AssignExpr
pInteger = Constant <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser AssignExpr
pTerm = choice @[]
  [ parens pExpr
  , pVariable
  , pInteger
  ]

pExpr :: Parser AssignExpr
pExpr = makeExprParser pTerm operatorTable

pAssign :: Parser Assign
pAssign = do
  var <- myVar
  symbol "="
  expr <- pExpr
  return $ Assign var expr

pAssignments :: Parser [Assign]
pAssignments = some pAssign

parseString :: String -> [Assign]
parseString = parseString' "console input"

parseString' :: String -> String -> [Assign]
parseString' fileName s = fromRight [] (runParser pAssignments fileName (pack s))

parseFromFile :: String -> IO [Assign]
parseFromFile filename = parseString' filename <$> (readFile filename)

-- runParser pAssignments "console input" "x=4 x=2*3"
