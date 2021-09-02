{-# LANGUAGE ScopedTypeVariables #-}

module Kriti.Parser (
  Accessor(..),
  ValueExt(..),
  SourcePosition(..),
  Span,
  ParseError,
  parser,
  renderPath,
  parsePath,
) where

import Kriti.Error                   (Error(..), ErrorCode(..), SourcePosition(..), Span(..), ToError(..), TryFromError(..), fromSourcePos, incCol)
import Kriti.Lexer                   (Token(..), TokenExt(..))

import Control.Applicative           ((<|>), many)
import Control.Monad                 (guard)
import Control.Monad.Identity        (Identity)
import Data.Bifunctor                (first)
import Data.Dynamic                  (toDyn)
import Data.Function                 ((&))
import Data.Monoid                   (Alt(..))
import Data.Scientific               (Scientific, toBoundedInteger)
import Data.Text                     (Text)

import qualified Data.Aeson          as J
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Text.Parsec         as P
import qualified Text.Parsec.Error as PE

-- | TODO: Documentation.
data Accessor =
    Obj Text
  | Arr Int
  deriving stock (Eq, Read, Show)

-- | TODO: Documentation.
renderAccessor :: Accessor -> Text
renderAccessor = \case
  Obj txt -> txt
  Arr i -> T.pack $ show i

-- | TODO: Documentation.
renderPath :: [(Span, Accessor)] -> Text
renderPath path = T.intercalate "." $ map (renderAccessor . snd) path

-- | TODO: Documentation.
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

-- | TODO: Documentation.
type Parser a = P.ParsecT [TokenExt] () Identity a

-- | TODO: Documentation.
match :: (TokenExt -> Maybe a) -> Parser a
match = P.token (show . teType) tePos

-- | TODO: Documentation.
match_ :: (Token -> Bool) -> Parser ()
match_ f = match (guard . f . teType)

-- | TODO: Documentation.
colon :: Parser ()
colon = match_ (== Colon)

-- | TODO: Documentation.
dot :: Parser ()
dot = match_ (== Dot)

-- | TODO: Documentation.
comma :: Parser ()
comma = match_ (== Comma)

-- | TODO: Documentation.
openCurly :: Parser ()
openCurly = match_ (== CurlyOpen)

-- | TODO: Documentation.
closeCurly :: Parser ()
closeCurly = match_ (== CurlyClose)

-- | TODO: Documentation.
squareOpen :: Parser ()
squareOpen = match_ (== SquareOpen)

-- | TODO: Documentation.
squareClose :: Parser ()
squareClose = match_ (== SquareClose)

-- | TODO: Documentation.
bling :: Parser ()
bling = match_ (== Bling)

-- | TODO: Documentation.
underscore :: Parser ()
underscore = match_ (== Underscore)

-- | TODO: Documentation.
assignment :: Parser ()
assignment = match_ (== Assignment)

-- | TODO: Documentation.
ident :: Parser Text
ident = match \case
  TokenExt (Identifier s) _ -> Just s
  _ -> Nothing

-- | TODO: Documentation.
ident_ :: Text -> Parser ()
ident_ s = match_ \case
  Identifier s' -> s == s'
  _ -> False

-- | TODO: Documentation.
bool :: Parser Bool
bool = match \case
  TokenExt (BoolLit p) _ -> Just p
  _ -> Nothing

-- | TODO: Documentation.
stringLit :: Parser Text
stringLit = match \case
  TokenExt (StringLit s) _ -> Just s
  _ -> Nothing

-- | TODO: Documentation.
number :: Fractional a => Parser a
number = match \case
  TokenExt (NumLit n) _ -> Just (fromRational $ toRational n)
  _ -> Nothing

-- | TODO: Documentation.
integer :: Parser Int
integer = match \case
  TokenExt (NumLit n) _ -> toBoundedInteger n
  _ -> Nothing

-- | TODO: Documentation.
template :: Parser a -> Parser a
template = P.between (openCurly *> openCurly) (closeCurly *> closeCurly)

-- | TODO: Documentation.
parens :: Parser a -> Parser a
parens = P.between (match_ (== ParenOpen)) (match_ (== ParenClose))

-- | TODO: Documentation.
parseNull :: Parser ValueExt
parseNull = do
  ident_ "null" <|> P.try (openCurly *> closeCurly)
  pure Null

-- | TODO: Documentation.
parseString :: Parser ValueExt
parseString = String <$> stringLit

-- | TODO: Documentation.
parseNumber :: Parser ValueExt
parseNumber = Number <$> number

-- | TODO: Documentation.
parseBool :: Parser ValueExt
parseBool = Boolean <$> bool

-- | TODO: Documentation.
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

-- | TODO: Documentation.
blingPrefixedId :: Parser Text
blingPrefixedId = do
  bling
  x <- P.optionMaybe ident
  case x of
    Just x' -> pure $ "$" <> x'
    Nothing -> pure "$"

-- | TODO: Documentation.
parsePath :: Parser ValueExt
parsePath = do
  startPos <- fromSourcePos <$> P.getPosition
  startAccessor <- prefix <|> fmap Obj ident
  rest <- many $ obj <|> arr
  let origPath = (startPos, startAccessor) : rest
      path = origPath & map \(pos, accessor) ->
        let aSpan = Span pos (Just $ incCol (len accessor) pos)
        in (aSpan, accessor)
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

-- | TODO: Documentation.
parseArray :: Parser ValueExt
parseArray = do
  squareOpen
  xs <- parseJson `P.sepBy` comma
  squareClose
  pure $ Array $ V.fromList xs

-- | TODO: Documentation.
parseRange :: Parser ValueExt
parseRange = do
  pos1 <- fromSourcePos <$> P.getPosition
  (idx, bndr, Path path) <- range
  body <- parseJson
  end'
  pos2 <- fromSourcePos <$> P.getPosition
  let aSpan = Span pos1 (Just pos2)
  pure $ Range aSpan idx bndr path body
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

-- | TODO: Documentation.
parserIff :: Parser ValueExt
parserIff = do
  pos1 <- fromSourcePos <$> P.getPosition
  p <- template $ ident_ "if" *> parsePath
  t1 <- parseJson
  template $ ident_ "else"
  t2 <- parseJson
  template $ ident_ "end"
  pos2 <- fromSourcePos <$> P.getPosition
  let aSpan = Span pos1 (Just pos2)
  pure $ Iff aSpan p t1 t2

-- | TODO: Documentation.
parseJson :: Parser ValueExt
parseJson = do
  e1 <- start
  mE2 <- end
  case mE2 of
    Nothing -> pure e1
    Just (f, e2) -> pure (f e1 e2)

-- | TODO: Documentation.
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

-- | TODO: Documentation.
end :: Parser (Maybe (ValueExt -> ValueExt -> ValueExt, ValueExt))
end = lt <|> gt <|> eq <|> pure Nothing
  where
    lt = match_ (== LT') *> parseJson >>= (pure . Just . (Lt, ))
    gt = match_ (== GT') *> parseJson >>= (pure . Just . (Gt, ))
    eq = match_ (== Eq') *> parseJson >>= (pure . Just . (Eq, ))

-- | TODO: Documentation.
newtype ParseError = ParseError P.ParseError
  deriving stock (Show)

instance ToError ParseError where
  toError thisError@(ParseError err) =
    let startPos = fromSourcePos $ PE.errorPos err
        errorMessage = PE.showErrorMessages
          "or"
          "unknown parse error"
          "expecting"
          "unexpected"
          "end of input" $
          PE.errorMessages err
    in
      Error {
        _code = ParseErrorCode,
        _message = T.pack errorMessage,
        _span = Span startPos Nothing,
        _innerError = toDyn thisError
      }

-- Use the default instance, which tries to cast the ''Dynamic' to a
-- 'ParseError'.
instance TryFromError ParseError

getSourcePos :: Parser SourcePosition
getSourcePos = fromSourcePos <$> P.getPosition

parser :: [TokenExt] -> Either ParseError ValueExt
parser = first ParseError . P.runParser parseJson mempty mempty
