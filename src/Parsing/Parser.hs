module Parsing.Parser where

import "parsec" Text.Parsec
import "parsec" Text.Parsec.Char
import "parsec" Text.Parsec.Token
import "parsec" Text.Parsec.Expr

import "base" Data.Functor
import "base" Data.List

defCommentStart :: String
defCommentStart = "{-"
defCommentEnd :: String
defCommentEnd = "-}"
defCommentLine :: String
defCommentLine = "--"
defNestedComments :: Bool
defNestedComments = True
illegalChars :: [Char]
illegalChars = "()_"
defReservedNames :: [String]
defReservedNames = ["expression","lassoc","rassoc","nassoc","_"]
defCaseSensitive :: Bool
defCaseSensitive = True

--this is a language def solely to extract operator precedence from a file. Only reason for language def is to properly skip comments
optblLangDef :: (Stream s m Char) => GenLanguageDef s u m
optblLangDef = LanguageDef {
    commentStart = defCommentStart
  , commentEnd = defCommentEnd
  , commentLine = defCommentLine
  , nestedComments = defNestedComments
  , identStart = noneOf illegalChars
  , identLetter = noneOf illegalChars
  --there should be no operators in the language
  , opStart = pure ' '
  , opLetter = pure ' '
  , reservedNames = defReservedNames
  , reservedOpNames = []
  , caseSensitive = defCaseSensitive
}

tpLD :: (Stream s m Char) => GenTokenParser s u m
tpLD = makeTokenParser optblLangDef



toMixfixParser :: (Stream s m Char) => [Maybe String] -> (String -> ParsecT s u m t) -> ParsecT s u m t -> ParsecT s u m [t]
toMixfixParser lst reserved term = sequence (toParser <$> lst)
  where toParser Nothing = term
        toParser (Just n) = reserved n

templateParser :: (Stream s m Char) =>
    GenTokenParser s u m ->
    ParsecT s u m [Maybe String]
templateParser tp = many1 $
  ((lexeme tp $ reserved tp "_") $> Nothing)
  <|> Just <$> identifier tp

assoc :: (Stream s m Char) =>
    GenTokenParser s u m ->
    ParsecT s u m Assoc
assoc tp = (reserved tp "nassoc" $> AssocNone)
          <|> (reserved tp "lassoc" $> AssocLeft)
          <|> (reserved tp "rassoc" $> AssocRight)

data MixFixDecl = MixFixDecl {
    template :: [Maybe String]
  , associativity :: Assoc
  , prescedence :: Integer
}

mixfixDeclaration :: (Stream s m Char) =>
    GenTokenParser s u m ->
    ParsecT s u m MixFixDecl
mixfixDeclaration tp = do
  reserved tp "expression"
  asc <- assoc tp
  presc <- natural tp
  tmp <- templateParser tp
  return $ MixFixDecl { template = tmp
                      , associativity = asc
                      , prescedence = presc}
{-
mixfixTermParser :: [MixFixDecl] -> (t -> t -> t) -> ParsecT s u m t
mixfixTermParser decls appl =
  where sortDecls = reverse $ sortOn prescedence decls
        toParser mfd = case prescedence mfd of
                          AssocNone -> _--problem: term parser unknown (should include this one)
                          AssocLeft -> chainl1 _ (pure appl)
                          AssocRight -> chainr1 _ (pure appl)
-}
