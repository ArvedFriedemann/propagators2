module Parsing.Parser where

import "base" Data.Functor
import "base" Data.List
import "base" Data.Maybe
import "base" Control.Monad

import "parsec" Text.Parsec
import "parsec" Text.Parsec.Token


illegalChars :: String
illegalChars = ";_ \n\r\t"

--this is a language def solely to extract operator precedence from a file. Only reason for language def is to properly skip comments
optblLangDef :: Stream s m Char => GenLanguageDef s u m
optblLangDef = LanguageDef 
    { commentStart = "{-"
    , commentEnd = "-}"
    , commentLine = "--"
    , nestedComments = True
    , identStart = noneOf illegalChars
    , identLetter = noneOf illegalChars
    --there should be no operators in the language
    , opStart = pure ' '
    , opLetter = pure ' '
    , reservedNames = ["expression","lassoc","rassoc","nassoc","_",";"]
    , reservedOpNames = []
    , caseSensitive = True
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
toMixfixParser tp lst conc termsymb term = conc <$> sequence (toParser <$> lst)
  where toParser Nothing = term
        toParser (Just n) = termsymb <$> (lexeme tp $ symbol tp n)


backToMixfix :: [Maybe String] -> String
backToMixfix lst = concat $ f <$> lst
  where f Nothing = "_"
        f (Just s) = s

allNames :: [Maybe String] -> [String]
allNames = catMaybes

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

mixfixDeclarationsParser :: forall s u m. Stream s m Char => ParsecT s u m ([MixFixDecl], GenTokenParser s u m)
mixfixDeclarationsParser = do
  let sep = reserved tpLD ";"
  tbl <- concat <$> flip sepEndBy sep
                            ((pure <$> mixfixDeclaration tpLD)
                            <|> (many (notFollowedBy sep >> anyChar) $> []))
  let names = reservedNames (optblLangDef :: GenLanguageDef s u m) ++ concatMap allNames (template <$> tbl)
      reservedParsers = choice (try . symbol tpLD <$> names)--TODO: notFollowedBy something reserved for valid identifier letters
  return (tbl, makeTokenParser $ optblLangDef{
        reservedNames = names
      , identStart = (notFollowedBy $ reservedParsers) >> noneOf illegalChars
      , identLetter = (notFollowedBy $ reservedParsers) >> noneOf illegalChars
      })

nextLine :: (Stream s m Char) => ParsecT s u m ()
nextLine = void $ manyTill anyChar (char '\n')

eol :: (Stream s m Char) => ParsecT s u m ()
eol = void (char '\n') <|> eof

mixfixTermParser :: forall s u m t . (Stream s m Char, Show t) =>
    GenTokenParser s u m ->
    [MixFixDecl] -> ([t] -> t) -> (String -> t) -> ParsecT s u m t -> ParsecT s u m t
mixfixTermParser tp decls conc atomicTerm initTerm = recparse
  where sortDecls = reverse $ sortOn prescedence decls
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
                              return (\x y -> conc [conc [x,t],y])
                              )
                            AssocRight -> chainr1 term (do
                              t <- mfp
                              --TODO: is this correct?
                              return (\x y -> conc [x,conc [t,y]])
                              )

-----------------------------------
--KB Parsing
-----------------------------------
parseKB :: (Stream s m Char, Show t) =>
    ([t] -> t) -> (String -> t) -> (String -> t) -> ParsecT s u m ([t],GenTokenParser s u m)
parseKB conc constant variable = do
  whiteSpace tpLD
  (decls, tp) <- lookAhead mixfixDeclarationsParser
  --traceM $ show decls

  let sep = lexeme tp $ symbol tp ";"
  --TODO: distinguish between vars and constants!
  ts <- concat <$> ((flip sepEndBy) sep $
    (do
      notFollowedBy $ (mixfixDeclaration tpLD $> "mixfix declaration")
      pure <$> mixfixTermParser tp decls conc constant (variable <$> (lexeme tp $ identifier tp))
    ) <|> (many ((notFollowedBy sep) >> anyChar) $> []) )

  return (ts, tp)
