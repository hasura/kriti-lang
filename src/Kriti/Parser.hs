module Kriti.Parser ( Accessor(..)
                    , ValueExt(..)
                    , SourcePosition(..)
                    , Span
                    , ParseError
                    , parser
                    , renderPath
                    , parsePath
                    ) where

import Kriti.Error
import Kriti.Lexer

import Control.Applicative
import Control.Monad.Identity
import Data.Bifunctor                (first)
import Data.Function                 ((&))
import Data.List                     (intersperse)
import Data.Monoid                   (Alt(..))
import Data.Scientific               (Scientific, toBoundedInteger)
import Data.Text                     (Text)

import qualified Data.Aeson          as J
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Text.Parsec         as P
import qualified Text.Parsec.Error as PE

data Accessor = Obj Text | Arr Int
  deriving (Show, Eq, Read)

renderAccessor :: Accessor -> Text
renderAccessor = \case
  Obj txt -> txt
  Arr i -> T.pack $ show i

renderPath :: [(Span, Accessor)] -> Text
renderPath = mconcat . intersperse "." . fmap (renderAccessor . snd)

data ValueExt =
  -- Core Aeson Terms
    Object (M.HashMap Text ValueExt)
  | Array (V.Vector ValueExt)
  | String Text
  | Number Scientific
  | Boolean Bool
  | Null
  -- Extended Terms
  | Path [(Span, Accessor)]
  | Iff Span ValueExt ValueExt ValueExt
  | Eq ValueExt ValueExt
  | Gt ValueExt ValueExt
  | Lt ValueExt ValueExt
  | AND Span ValueExt ValueExt
  | OR Span ValueExt ValueExt
  | Member Span ValueExt ValueExt
  | Range Span (Maybe Text) Text [(Span, Accessor)] ValueExt
  -- ^ {{ range i, x := $.foo.bar }}
  deriving (Show, Eq, Read)

instance J.FromJSON ValueExt where
  parseJSON = \case
    J.Null       -> pure   Null
    J.String s   -> pure $ String s
    J.Number i   -> pure $ Number i
    J.Bool p     -> pure $ Boolean p
    J.Array arr  -> Array <$> traverse J.parseJSON arr
    J.Object obj | null obj -> pure Null
    J.Object obj -> Object <$> traverse J.parseJSON obj

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

template :: Parser a -> Parser a
template = P.between (openCurly *> openCurly) (closeCurly *> closeCurly)

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

blingPrefixedId :: Parser Text
blingPrefixedId = do
  bling
  x <- P.optionMaybe ident
  case x of
    Just x' -> pure $ "$" <> x'
    Nothing -> pure "$"

parsePath :: Parser ValueExt
parsePath = do
  startPos <- fromSourcePos <$> P.getPosition
  x <- prefix <|> fmap Obj ident
  xs <- many $ obj <|> arr
  let path = ((startPos, x):xs) & fmap \(pos, el) -> ((pos, Just $ incCol (len el) pos), el)
  pure $ Path path
  where
    len (Obj x) = T.length x
    len (Arr i) = length (show i) + 1
    prefix = P.try $ Obj <$> blingPrefixedId
    arr = do
      pos <- getSourcePos
      squareOpen
      x <- Arr <$> integer
      squareClose
      pure (pos, x)
    obj = do
      pos <- getSourcePos
      dot
      x <- Obj <$> ident
      pure (pos, x)

parseArray :: Parser ValueExt
parseArray = do
  squareOpen
  xs <- parseJson `P.sepBy` comma
  squareClose
  pure $ Array $ V.fromList xs

parseRange :: Parser ValueExt
parseRange = do
  pos1 <- fromSourcePos <$> P.getPosition
  (idx, bndr, Path path) <- range
  body <- parseJson
  end'
  pos2 <- fromSourcePos <$> P.getPosition
  pure $ Range (pos1, Just pos2) idx bndr path body
  where
    range = template $ do
      ident_ "range"
      idx <- (Just <$> (blingPrefixedId <|> ident)) <|> (Nothing <$ underscore)
      comma
      bndr <- blingPrefixedId <|> ident
      assignment
      path <- parsePath
      pure (idx, bndr, path)
    end' = template (ident_ "end")

parserIff :: Parser ValueExt
parserIff = do
  pos1 <- fromSourcePos <$> P.getPosition
  p <- template $ ident_ "if" *> parsePath
  t1 <- parseJson
  template $ ident_ "else"
  t2 <- parseJson
  template $ ident_ "end"
  pos2 <- fromSourcePos <$> P.getPosition
  pure $ Iff (pos1, Just pos2) p t1 t2

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
    -- NOTE: This isn't a very elegant solution. It would be better to
    -- factor out the initial `{` but `parseRange` and `parseIff` have
    -- nested `template` parsers which makes this difficult.
    , P.try parseObject
    , P.try (template parsePath)
    , P.try parseRange
    , parserIff
    , parens parseJson
    ]

end :: Parser (Maybe (ValueExt -> ValueExt -> ValueExt, ValueExt))
end = lt <|> gt <|> eq <|> pure Nothing
  where
    lt = match_ (== LT') *> parseJson >>= (pure . Just . (Lt, ))
    gt = match_ (== GT') *> parseJson >>= (pure . Just . (Gt, ))
    eq = match_ (== Eq') *> parseJson >>= (pure . Just . (Eq, ))

newtype ParseError = ParseError P.ParseError
  deriving Show

instance RenderError ParseError where
  render (ParseError err) =
    let startPos = fromSourcePos $ PE.errorPos err
        errorMessage = PE.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" $ PE.errorMessages err
    in RenderedError { _code = ParseErrorCode, _message = T.pack errorMessage, _span = (startPos, Nothing) }

getSourcePos :: Parser SourcePosition
getSourcePos = fromSourcePos <$> P.getPosition

parser :: [TokenExt] -> Either ParseError ValueExt
parser = first ParseError . P.runParser parseJson mempty mempty
