{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Kriti.Parser where --( Accessor(..)
--, ValueExt(..)
--, SourcePosition(..)
--, Span
--, ParseError
--, parser
--, parserAndLexer
--, renderPath
--, parsePath
--, parserStringInterp
--) where

import Control.Applicative
import Control.Lens hiding (Context, op)
import Control.Monad
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import Data.Either (lefts, rights)
import Data.Foldable
import qualified Data.HashMap.Strict as M
import Data.List (intersperse)
import Data.Monoid (Alt (..))
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Kriti.Error
import qualified Kriti.Lexer as Lex
import qualified Kriti.Lexer.Token as Lex
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Error as PE

data Accessor = Obj Text | Arr Int
  deriving (Show, Eq, Read)

renderAccessor :: Accessor -> Text
renderAccessor = \case
  Obj txt -> txt
  Arr i -> T.pack $ show i

renderPath :: [(Span, Accessor)] -> Text
renderPath = mconcat . intersperse "." . fmap (renderAccessor . snd)

data ValueExt
  = -- Core Aeson Terms
    Object (M.HashMap Text ValueExt)
  | Array (V.Vector ValueExt)
  | String Text
  | Number Scientific
  | Boolean Bool
  | Null
  | -- Extended Terms
    StringInterp Span [ValueExt]
  | Path [(Span, Accessor)]
  | Iff Span ValueExt ValueExt ValueExt
  | Eq Span ValueExt ValueExt
  | Gt Span ValueExt ValueExt
  | Lt Span ValueExt ValueExt
  | And Span ValueExt ValueExt
  | Or Span ValueExt ValueExt
  | Member Span ValueExt ValueExt
  | Range Span (Maybe Text) Text [(Span, Accessor)] ValueExt
  | EscapeURI Span ValueExt
  deriving (Show, Eq, Read)

instance J.FromJSON ValueExt where
  parseJSON = \case
    J.Null -> pure Null
    J.String s -> pure $ String s
    J.Number i -> pure $ Number i
    J.Bool p -> pure $ Boolean p
    J.Array arr -> Array <$> traverse J.parseJSON arr
    J.Object obj | null obj -> pure Null
    J.Object obj -> Object <$> traverse J.parseJSON obj

-- {{ range $index, $article := .event.author.articles }}

type Parser = P.Parsec Lex.LexError Lex.TokenStream

match :: (Lex.Token -> Maybe a) -> Parser a
match f = P.try $ do
  tokenExt@Lex.TokenExt {teType} <- P.anySingle
  case f teType of
    Nothing -> P.unexpected (PE.Tokens $ pure tokenExt)
    Just a -> pure a

match_ :: (Lex.Token -> Bool) -> Parser ()
match_ f = P.satisfy (f . Lex.teType) >> pure ()

eitherP :: Parser a -> Parser b -> Parser (Either a b)
eitherP a b = Left <$> a <|> Right <$> b

colon :: Parser ()
colon = match_ (== Lex.Colon)

dot :: Parser ()
dot = match_ (== Lex.Dot)

comma :: Parser ()
comma = match_ (== Lex.Comma)

openCurly :: Parser ()
openCurly = match_ (== Lex.CurlyOpen)

closeCurly :: Parser ()
closeCurly = match_ (== Lex.CurlyClose)

squareOpen :: Parser ()
squareOpen = match_ (== Lex.SquareOpen)

squareClose :: Parser ()
squareClose = match_ (== Lex.SquareClose)

bling :: Parser ()
bling = match_ (== Lex.Bling)

underscore :: Parser ()
underscore = match_ (== Lex.Underscore)

assignment :: Parser ()
assignment = match_ (== Lex.Assignment)

reservedWords :: [Text]
reservedWords = [ "escapeUri" ]

ident :: Parser Text
ident = match \case
  Lex.Identifier s | s `notElem` reservedWords -> Just s
  _ -> Nothing

ident_ :: Text -> Parser ()
ident_ s = match_ \case
  Lex.Identifier s' | s' `notElem` reservedWords -> s == s'
  _ -> False

reserved :: Parser Text
reserved = match \case
  Lex.Identifier s | s `elem` reservedWords -> Just s
  _ -> Nothing

reserved_ :: Text -> Parser ()
reserved_ s = match_ \case
  Lex.Identifier s' | s' `elem` reservedWords -> s == s'
  _ -> False

bool :: Parser Bool
bool = match \case
  Lex.BoolLit p -> Just p
  _ -> Nothing

stringLit :: Parser Text
stringLit = match \case
  Lex.StringTem s -> Just s
  _ -> Nothing

stringTem :: Parser [Either Text Lex.TokenStream]
stringTem = do
  res <- match \case
    Lex.StringTem s -> Just $ traverse (traverse Lex.lexer) $ splitText s
    _ -> Nothing
  case res of
    Left err -> P.customFailure err
    Right tem -> pure tem

-- | Split Template Literal into unlexed string literals and unlexed
-- expressions.
--
-- We could use a parser combinators here but at this point they don't
-- provide much advantage. If the template syntax ${..} is incorrect
-- then it will be treated as a string literal and the content of the
-- template expressions is lexed and parsed downstream from this
-- function call.
--
-- Using parser combinators would require writing a second Parser
-- alias and a run function:
--
-- type ParserTemplate = P.Parsec Void Text
-- runTemParser :: Text -> Either TemplateParseError [Either Text Text]
--
-- `runTemParser` would run the parser then convert the
-- `ParseErrorBundle` into a custom error type `TemplateParseError`
-- which we can then register with our main parser as a custom error
-- field.
--
-- If we are not okay with bad template syntax, eg., `foo${bar`, being
-- treated as string literals then we ought switch to parser
-- combinators here.
splitText :: Text -> [Either Text Text]
splitText t = reverse $ go t []
  where
    go str acc
      | T.null str = acc
      | let (txt, rest) = T.breakOn "\\{" str,
        not (T.null rest) =
        Left (txt <> T.drop 1 rest) : acc
      | let (_, rest) = T.breakOn "{{" str,
        T.null rest || T.length rest == 2 =
        Left str : acc
      | let (txt, str') = T.breakOn "{{" str
            (expr, rest) = T.breakOn "}}" (T.drop 2 str'),
        not (T.null expr) && not (T.null rest) =
        go (T.drop 2 rest) (Right expr : Left txt : acc)
      | otherwise = Left str : acc

number :: Fractional a => Parser a
number = match \case
  Lex.NumLit _ n -> Just (fromRational $ toRational n)
  _ -> Nothing

integer :: Parser Int
integer = match \case
  Lex.NumLit _ n -> toBoundedInteger n
  _ -> Nothing

betweenCurly :: Parser a -> Parser a
betweenCurly = P.between openCurly closeCurly

betweenParens :: Parser a -> Parser a
betweenParens = P.between (match_ (== Lex.ParenOpen)) (match_ (== Lex.ParenClose))

template :: Parser a -> Parser a
template = P.between (openCurly *> openCurly) (closeCurly *> closeCurly)

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
  x <- optional ident
  case x of
    Just x' -> pure $ "$" <> x'
    Nothing -> pure "$"

parsePath :: Parser ValueExt
parsePath = do
  startPos <- fromSourcePos <$> P.getSourcePos
  x <- prefix <|> fmap Obj ident
  xs <- many $ obj <|> arr
  let path = ((startPos, x) : xs) & fmap \(pos, el) -> ((pos, Just $ incCol (len el) pos), el)
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
  pos1 <- fromSourcePos <$> P.getSourcePos
  (idx, bndr, Path path) <- range
  body <- parseJson
  end'
  pos2 <- fromSourcePos <$> P.getSourcePos
  pure $ Range (pos1, Just pos2) idx bndr path body
  where
    range = template $ do
      ident_ "range"
      idx <- Just <$> (blingPrefixedId <|> ident) <|> Nothing <$ underscore
      comma
      bndr <- blingPrefixedId <|> ident
      assignment
      path <- parsePath
      pure (idx, bndr, path)
    end' = template (ident_ "end")

parserIff :: Parser ValueExt
parserIff = do
  pos1 <- fromSourcePos <$> P.getSourcePos
  p <- template $ ident_ "if" *> parsePath
  t1 <- parseJson
  template $ ident_ "else"
  t2 <- parseJson
  template $ ident_ "end"
  pos2 <- fromSourcePos <$> P.getSourcePos
  pure $ Iff (pos1, Just pos2) p t1 t2

registerParseErrorBundle :: P.MonadParsec e s m => P.ParseErrorBundle s e -> m ()
registerParseErrorBundle (P.ParseErrorBundle errs _) =
  traverse_ P.registerParseError errs

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

removeRights :: [Either a b] -> [a]
removeRights ls = [x | Left x <- ls]

parserStringInterp :: Parser ValueExt
parserStringInterp = do
  pos1 <- fromSourcePos <$> P.getSourcePos
  tem <- stringTem
  pos2 <- fromSourcePos <$> P.getSourcePos
  -- TODO: Recursively parsing like this will result in a junk
  -- `SourcePosition`
  let p = first ParseError . P.runParser parsePath mempty
  x <- traverse (pure . traverse p) tem

  let errs = lefts x
  traverse_ (registerParseErrorBundle . _peErrorBundle) errs

  let vals = rights x
  let vals' = either String id <$> vals
  if all isLeft vals
    then pure $ String $ foldl (<>) mempty $ removeRights vals
    else pure $ StringInterp (pos1, Just pos2) vals'

parseEscape :: Parser ValueExt
parseEscape = do
  pos1 <- fromSourcePos <$> P.getSourcePos
  reserved_ "escapeUri"
  t1 <- parseJson
  pos2 <- fromSourcePos <$> P.getSourcePos
  pure $ EscapeURI (pos1, Just pos2) t1

parseJson :: Parser ValueExt
parseJson = do
  e1 <- start
  mE2 <- end
  case mE2 of
    Nothing -> pure e1
    Just (f, e2) -> pure (f e1 e2)

start :: Parser ValueExt
start =
  getAlt $
    foldMap
      Alt
      [ parseNull,
        parserStringInterp,
        parseNumber,
        parseBool,
        parseArray,
        -- NOTE: This isn't a very elegant solution. It would be better to
        -- factor out the initial `{` but `parseRange` and `parseIff` have
        -- nested `template` parsers which makes this difficult.
        P.try parseObject,
        P.try (template parsePath),
        P.try parseRange,
        P.try (template parseEscape),
        parserIff,
        betweenParens parseJson
      ]

end :: Parser (Maybe (ValueExt -> ValueExt -> ValueExt, ValueExt))
end = lt <|> gt <|> eq <|> and' <|> or' <|> pure Nothing
  where
    lt, gt, eq :: Parser (Maybe (ValueExt -> ValueExt -> ValueExt, ValueExt))
    lt = op Lex.Lt Lt 0
    gt = op Lex.Gt Gt 0
    eq = op Lex.Eq Eq 1
    and' = op Lex.And And 1
    or' = op Lex.Or Or 1

    op tok con size = do
      pos1 <- getSourcePos
      void $ match_ (== tok)
      v <- parseJson
      pure $ Just (con (pos1, Just $ incCol size pos1), v)

newtype ParseError = ParseError {_peErrorBundle :: P.ParseErrorBundle Lex.TokenStream Lex.LexError}
  deriving (Show)

instance RenderError ParseError where
  render (ParseError err) =
    let startPos = fromSourcePos $ P.pstateSourcePos $ P.bundlePosState err
        errorMessage = P.errorBundlePretty err
     in RenderedError {_code = ParseErrorCode, _message = T.pack errorMessage, _span = (startPos, Nothing)}

getSourcePos :: Parser SourcePosition
getSourcePos = fromSourcePos <$> P.getSourcePos

parser :: Lex.TokenStream -> Either ParseError ValueExt
parser = first ParseError . P.runParser parseJson mempty

parserAndLexer :: Text -> Either RenderedError ValueExt
parserAndLexer t = do
  lexemes <- first render $ Lex.lexer t
  first render $ parser lexemes
