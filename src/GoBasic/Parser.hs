module GoBasic.Parser where

import GoBasic.Lexer

import Control.Applicative
import Control.Monad.Identity
import Data.Monoid (Alt(..))
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)

import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Text.Parsec as P

data Accessor = Obj Text | Arr Int
  deriving (Show, Eq)

data ValueExt =
  -- Core Aeson Terms
    Object (M.HashMap Text ValueExt)
  | Array (V.Vector ValueExt)
  | String Text
  | Number Scientific
  | Boolean Bool
  | Null
  -- Extended Terms
  | Path [Accessor]
  | Iff ValueExt ValueExt ValueExt
  | Eq ValueExt ValueExt
  | Gt ValueExt ValueExt
  | Lt ValueExt ValueExt
  | AND ValueExt ValueExt
  | OR ValueExt ValueExt
  | Member ValueExt ValueExt
  | Range (Maybe Text) Text [Accessor] ValueExt
  -- ^ {{ range i, x := $.foo.bar }}
  -- ^ {{ range $.foo.bar }}
  deriving (Show, Eq)

-- {{ range $index, $article := .event.author.articles }}

type Parser a = P.ParsecT [TokenExt] () Identity a

match :: (TokenExt -> Maybe a) -> Parser a
match = P.token (show . teType) tePos

match_ :: (Token -> Bool) -> Parser ()
match_ f = match (guard . f . teType)

colon :: Parser ()
colon = match_ (== Colon)

dot :: Parser ()
dot = match_ (== Dot)

comma :: Parser ()
comma = match_ (== Comma)

openCurly :: Parser ()
openCurly = match_ (== CurlyOpen)

closeCurly :: Parser ()
closeCurly = match_ (== CurlyClose)

templateOpen :: Parser ()
templateOpen = match_ (== TemplateOpen)

templateClose :: Parser ()
templateClose = match_ (== TemplateClose)

squareOpen :: Parser ()
squareOpen = match_ (== SquareOpen)

squareClose :: Parser ()
squareClose = match_ (== SquareClose)

bling :: Parser ()
bling = match_ (== Bling)

underscore :: Parser ()
underscore = match_ (== Underscore)

assignment :: Parser ()
assignment = match_ (== Assignment)

ident :: Parser Text
ident = match \case
  TokenExt (Identifier s) _ -> Just s
  _ -> Nothing

ident_ :: Text -> Parser ()
ident_ s = match_ \case
  Identifier s' -> s == s'
  _ -> False

bool :: Parser Bool
bool = match \case
  TokenExt (BoolLit p) _ -> Just p
  _ -> Nothing

stringLit :: Parser Text
stringLit = match \case
  TokenExt (StringLit s) _ -> Just s
  _ -> Nothing

number :: Fractional a => Parser a
number = match \case
  TokenExt (NumLit n) _ -> Just (fromRational $ toRational n)
  _ -> Nothing

integer :: Parser Int
integer = match \case
  TokenExt (NumLit n) _ -> toBoundedInteger n
  _ -> Nothing

commaSep :: Parser a -> Parser [a]
commaSep p = p `P.sepBy` comma

template :: Parser a -> Parser a
template = P.between templateOpen templateClose

parens :: Parser a -> Parser a
parens = P.between (match_ (== ParenOpen)) (match_ (== ParenClose))

parseNull :: Parser ValueExt
parseNull = do
  ident_ "null" <|> P.try (openCurly *> closeCurly)
  pure Null

parseString :: Parser ValueExt
parseString = String <$> stringLit

parseNumber :: Parser ValueExt
parseNumber = Number <$> number

parseBool :: Parser ValueExt
parseBool = Boolean <$> bool

parseArray :: Parser ValueExt
parseArray = do
  squareOpen
  xs <- parseJson `P.sepBy` comma
  squareClose
  pure $ Array $ V.fromList xs

parseObject :: Parser ValueExt
parseObject = do
  openCurly
  keys <- parseField `P.sepBy1` comma
  closeCurly
  pure $ Object $ M.fromList keys
  where
    parseField :: Parser (Text, ValueExt)
    parseField = do
      key <- stringLit
      colon
      value <- parseJson
      pure (key, value)

parsePath :: Parser ValueExt
parsePath = do
  bling
  x <- P.try obj <|> arr
  xs <- many (obj <|> arr)
  pure $ Path (x:xs)
  where
    arr = squareOpen *> (Arr <$> integer) <* squareClose
    obj = dot *> fmap Obj ident

parseRange :: Parser ValueExt
parseRange = do
  (bndr, Path path) <- range
  body <- parseJson
  end'
  pure $ Range Nothing bndr path body
  where
    range = template $ do
      (ident_ "range")
      underscore
      comma
      bndr <- ident
      assignment
      path <- parsePath
      pure (bndr, path)
    end' = template (ident_ "end")

parserIff :: Parser ValueExt
parserIff = do
  p <- template $ ident_ "if" *> parsePath
  t1 <- parseJson
  template $ ident_ "else"
  t2 <- parseJson
  template $ ident_ "end"
  pure $ Iff p t1 t2

parseExt :: Parser ValueExt
parseExt = P.try (template parsePath) <|> P.try parseRange  <|> parserIff

parseJson :: Parser ValueExt
parseJson = do
  e1 <- start
  mE2 <- end
  case mE2 of
    Nothing -> pure e1
    Just (f, e2) -> pure (f e1 e2)

start :: Parser ValueExt
start =
  getAlt $ foldMap Alt
    [ parseNull
    , parseString
    , parseNumber
    , parseBool
    , parseArray
    , parseObject
    , parseExt
    , parens parseJson
    ]

end :: Parser (Maybe (ValueExt -> ValueExt -> ValueExt, ValueExt))
end = lt <|> gt <|> eq <|> pure Nothing
  where
    lt = match_ (== LT') *> parseJson >>= (pure . Just . (Lt, ))
    gt = match_ (== GT') *> parseJson >>= (pure . Just . (Gt, ))
    eq = match_ (== Eq') *> parseJson >>= (pure . Just . (Eq, ))
