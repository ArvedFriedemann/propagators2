module Parsing.Parser where

import "parsec" Text.Parsec
import "parsec" Text.Parsec.Char
import "parsec" Text.Parsec.Token
--import "parsec" Text.Parsec.Expr

import "base" Data.Functor
import "base" Data.List
import "base" Data.Maybe
import "base" Control.Monad
import "base" Debug.Trace

defCommentStart :: String
defCommentStart = "{-"
defCommentEnd :: String
defCommentEnd = "-}"
defCommentLine :: String
defCommentLine = "--"
defNestedComments :: Bool
defNestedComments = True
illegalChars :: [Char]
illegalChars = ";_ \n\r\t"
defReservedNames :: [String]
defReservedNames = ["expression","lassoc","rassoc","nassoc","_",";"]
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


{-
expression lassoc 10 _blah_,_ehn hwh_

concat [] y y
concat xs y zs -> concat (x : xs) y (x : zs)
-}

toMixfixParser :: (Stream s m Char) =>
    GenTokenParser s u m ->
    [Maybe String] -> ([t] -> t) -> (String -> t) -> ParsecT s u m t -> ParsecT s u m t
toMixfixParser tp lst conc termsymb term = do
  seqe <- concat <$> (sequence (toParser <$> lst))
  return $ conc $ (termsymb $ backToMixfix lst) : seqe
  where toParser Nothing = return <$> term
        toParser (Just n) = lexeme tp $ symbol tp n $> []--termsymb n

templateParser :: (Stream s m Char) =>
    GenTokenParser s u m ->
    ParsecT s u m [Maybe String]
templateParser tp = many1 $
  --weird that this is symbol and not reserved, but reserved needs a followup stopper apparently
  ((lexeme tp $ symbol tp "_") $> Nothing)
  <|> Just <$> identifier tp

data Assoc = AssocNone | AssocLeft | AssocRight
  deriving (Show, Eq, Ord)

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
} deriving (Show, Eq, Ord)

mixfixDeclaration :: (Stream s m Char) =>
    GenTokenParser s u m ->
    ParsecT s u m MixFixDecl
mixfixDeclaration tp = do
  reserved tp "expression"
  asc <- assoc tp
  presc <- natural tp
  tmp <- templateParser tp
  tmp' <- case asc of
    AssocNone -> pure tmp
    _ -> do
      guard (length tmp >= 2 &&
                  head tmp == Nothing &&
                  last tmp == Nothing )
                  <?> "Operator with associativity needs to be an infix"
      return (drop 1 . init $ tmp)
  return $ MixFixDecl { template = tmp'
                      , associativity = asc
                      , prescedence = presc}

mixfixDexlarationsParser :: (Stream s m Char) => ParsecT s u m ([MixFixDecl], GenTokenParser s u m)
mixfixDexlarationsParser = do
  let sep = reserved tpLD ";"
  tbl <- concat <$> flip sepEndBy sep
                            ((pure <$> mixfixDeclaration tpLD)
                            <|> (many (notFollowedBy sep >> anyChar) $> []))
  return (tbl, makeTokenParser $ optblLangDef{
      reservedNames = defReservedNames ++ (concatMap allNames (template <$> tbl))})

nextLine :: (Stream s m Char) => ParsecT s u m ()
nextLine = void $ manyTill anyChar (char '\n')

eol :: (Stream s m Char) => ParsecT s u m ()
eol = void (char '\n') <|> eof

mixfixTermParser :: forall s u m t . (Stream s m Char) =>
    GenTokenParser s u m ->
    [MixFixDecl] -> ([t] -> t) -> (String -> t) -> ParsecT s u m t -> ParsecT s u m t
mixfixTermParser tp decls conc atomicTerm initTerm = recparse
  where sortDecls = sortOn prescedence decls
        recparse :: ParsecT s u m t
        recparse = foldr (\fkt trm -> (try $ fkt trm recparse) <|> trm) initTerm (toParser <$> sortDecls)
        toParser :: MixFixDecl -> ParsecT s u m t -> ParsecT s u m t -> ParsecT s u m t
        toParser mfd term termTop = let mfp = toMixfixParser tp (template mfd) conc atomicTerm termTop
                        in do
                          case associativity mfd of
                            AssocNone -> mfp
                            AssocLeft -> chainl1 term (do
                              t <- mfp
                              --TODO: is this correct?
                              return (\x y -> conc [x,t,y])
                              )
                            AssocRight -> chainr1 term (do
                              t <- mfp
                              --TODO: is this correct?
                              return (\x y -> conc [x,t,y])
                              )

backToMixfix :: [Maybe String] -> String
backToMixfix lst = concat $ f <$> lst
  where f Nothing = "_"
        f (Just s) = s

allNames :: [Maybe String] -> [String]
allNames = catMaybes
