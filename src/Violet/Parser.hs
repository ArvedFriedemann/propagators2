{-# LANGUAGE OverloadedStrings #-}
module Violet.Parser where



import Control.Monad.Identity
import Control.Monad.Combinators.Expr
import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import Data.Void
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)


data Assign = Assign String Expr
  deriving (Eq, Ord, Show)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation ]
  , [ binary "*" Product
    , binary "/" Division
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)


sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "#")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

type Parser = Parsec Void Text

pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

myVar = lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
--pTerm = choice
--    ([parens pExpr
--    , pInteger
--] :: [Parser Expr])
pTerm = choice @[]
  [ parens pExpr
  , pVariable
  , pInteger
  ]
-- pTerm = (parens pExpr) <|> pVariable <|> pInteger


pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pAssign :: Parser Assign
pAssign = do
  var <- myVar
  symbol "="
  expr <- pExpr
  return $ Assign var expr
{-
x = 5
x = 2*x
def magic (x,y,z)
  return (x+y*u)

x = 5+ max(x,5,4)
-}

{-
[
  Assign "x" (Int 5),
  Assign "x" (Product (Int 2) (Var "x"))
]
-->
[
  Assign "x_0" (Int 5),
  Assign "x_1" (Product (Int 2) (Var "x_0"))
]

--get :: State s s
--put :: s -> State s ()

--Map.insert :: n -> k -> Map n k -> Map n k
--Map.lookup :: n -> Map n k -> Maybe k

(>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
(>>=) f m = \a -> (m (f a)) a




evalState :: State s a -> s -> a

evalState (clearNames e) (Map.empty)
-}

updateName :: String -> State (Map String Int) Int
updateName name = do
  mp <- get
  case Map.lookup name mp of
    Nothing -> do
      put $ Map.insert name 1 mp
      return 0
    Just i -> do
      put $ Map.insert name (i+1) mp
      return i

clearNames :: [Assign] -> State (Map String Int) [Assign]
clearNames [] = return []
clearNames (a@(Assign varName sth) : xs) = do
  var_numbering <- updateName varName
  let new_name = varName ++ (show var_numbering)
  rest <- clearNames xs
  return $ (Assign new_name sth) : rest




pAssignments :: Parser [Assign]
pAssignments = some pAssign
