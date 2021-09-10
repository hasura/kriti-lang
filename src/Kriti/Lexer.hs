module Kriti.Lexer where

import           Kriti.Error

import           Data.Char                    (isSpace)
import           Data.Maybe                   (maybeToList)
import           Data.Scientific              (Scientific, scientificP)
import           Data.Text                    (Text)
import           GHC.Generics
import           Text.Parsec.Pos              (SourcePos, incSourceColumn,
                                               incSourceLine, initialPos,
                                               newPos, setSourceColumn,
                                               sourceColumn, sourceLine)
import           Text.ParserCombinators.ReadP (ReadP, gather, readP_to_S)
import           Text.Read                    (lexP, lift, readPrec_to_P)
import           Text.Read.Lex.Extended       (lexString)

import qualified Data.Text                    as T
import qualified Text.Read.Lex                as L

data Token =
    StringLit Text
    -- ^ String Literal
  | Identifier Text
    -- ^ Identifier
  | NumLit Scientific
    -- ^ Number literal
  | BoolLit Bool
  | Bling
  | Colon
  | Dot
  | Comma
  | Eq
  | Gt
  | Lt
  | And
  | Or
  -- | Member
  | CurlyOpen
  | CurlyClose
  | SquareOpen
  | SquareClose
  | ParenOpen
  | ParenClose
  | Underscore
  | Assignment
  deriving (Show, Eq, Generic)

serialize :: Token -> Text
serialize = \case
    StringLit str   -> "\"" <> str <> "\""
    Identifier iden -> iden
    NumLit i        -> T.pack $ show i
    BoolLit True    -> "true"
    BoolLit False   -> "false"
    Bling           -> "$"
    Colon           -> ":"
    Dot             -> "."
    Comma           -> ","
    Eq              -> "=="
    Gt              -> ">"
    Lt              -> "<"
    And             -> "&&"
    Or              -> "||"
    CurlyOpen       -> "{"
    CurlyClose      -> "}"
    SquareOpen      -> "["
    SquareClose     -> "]"
    ParenOpen       -> "("
    ParenClose      -> ")"
    Underscore      -> "_"
    Assignment      -> ":="

data TokenExt = TokenExt { teType :: Token, tePos :: SourcePos }
  deriving (Show, Eq)

data LexError = LexError { lePos :: SourcePos }

instance RenderError LexError where
  render LexError{lePos} =
    RenderedError
      { _code = LexErrorCode
      , _message = "Invalid Lexeme"
      , _span = (fromSourcePos lePos, Nothing)
      }

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f b =  f b >>= \case
  Just (a, b') -> do
    as <- unfoldrM f b'
    pure $ a:as
  Nothing -> pure []

lexer :: Text -> Either LexError [TokenExt]
lexer t = unfoldrM go (t', iPos)
  where
    (t', iPos) = advance t (initialPos "sourceName") mempty
    go :: (Text, SourcePos) -> Either LexError (Maybe (TokenExt, (Text, SourcePos)))
    go (txt, pos)
      | T.null t = pure Nothing
      | Just s <- T.stripPrefix "true"  txt = stepLexer (BoolLit True) s pos
      | Just s <- T.stripPrefix "false" txt = stepLexer (BoolLit False) s pos
      | Just s <- T.stripPrefix "_"     txt = stepLexer Underscore s pos
      | Just s <- T.stripPrefix "."     txt = stepLexer Dot s pos
      | Just s <- T.stripPrefix ","     txt = stepLexer Comma s pos
      | Just s <- T.stripPrefix "$"     txt = stepLexer Bling s pos
      | Just s <- T.stripPrefix ":="    txt = stepLexer Assignment s pos
      | Just s <- T.stripPrefix ":"     txt = stepLexer Colon s pos
      | Just s <- T.stripPrefix "=="    txt = stepLexer Eq s pos
      | Just s <- T.stripPrefix ">"     txt = stepLexer Gt s pos
      | Just s <- T.stripPrefix "<"     txt = stepLexer Lt s pos
      | Just s <- T.stripPrefix "&&"    txt = stepLexer And s pos
      | Just s <- T.stripPrefix "||"    txt = stepLexer Or s pos
      | Just s <- T.stripPrefix "{"     txt = stepLexer CurlyOpen s pos
      | Just s <- T.stripPrefix "}"     txt = stepLexer CurlyClose s pos
      | Just s <- T.stripPrefix "["     txt = stepLexer SquareOpen s pos
      | Just s <- T.stripPrefix "]"     txt = stepLexer SquareClose s pos
      | Just s <- T.stripPrefix ")"     txt = stepLexer ParenClose s pos
      | Just s <- T.stripPrefix "("     txt = stepLexer ParenOpen s pos
      | Just (str, matched, s) <- stringLit txt  = pure $ Just (TokenExt (StringLit str) pos, advance s pos matched)
      | Just (str, matched, s) <- identifier txt = pure $ Just (TokenExt (Identifier str) pos, advance s pos matched)
      | Just (n, matched, s) <- numberLit txt    = pure $ Just (TokenExt (NumLit (realToFrac n)) pos, advance s pos matched)
      | otherwise = pure Nothing

    stepLexer :: Token -> Text -> SourcePos -> Either LexError (Maybe (TokenExt, (Text, SourcePos)))
    stepLexer tok s pos = pure $ Just (TokenExt tok pos, advance s pos (serialize tok))

    identifier :: Text -> Maybe (Text, Text, Text) -- (value, lit, remainder)
    identifier = fromRead (readPrec_to_P identLexeme 0) where
        identLexeme = do
          L.Ident s <- lexP
          pure (T.pack s)

    stringLit :: Text -> Maybe (Text, Text, Text) -- (value, lit, remainder)
    stringLit = fromRead (readPrec_to_P stringLexeme 0)
      where
        stringLexeme = do
          L.String s <- lift lexString
          pure (T.pack s)

    numberLit :: Text -> Maybe (Scientific, Text, Text) -- (value, lit, remainder)
    numberLit = fromRead scientificP

    fromRead :: ReadP a -> Text -> Maybe (a, Text, Text) -- (value, lit, remainder)
    fromRead rp txt =
      let matchS = maxParsed <$> readP_to_S (gather rp)
       in case matchS (T.unpack txt) of
            (((lit, value), rest):_) ->
              pure (value, T.pack lit, T.pack rest)
            _ -> Nothing

    -- | Choose the parse result which consumed the maximum number of bytes.
    maxParsed :: [(a, String)] -> [(a, String)]
    maxParsed xs =
      let f (a, str) = \case
            Just (a', str') -> if length str < length str' then pure (a, str) else pure (a', str')
            Nothing -> pure (a, str)
      in maybeToList $ foldr f Nothing xs

    advance :: Text -> SourcePos -> Text -> (Text, SourcePos)
    advance txt pos eaten =
      let (ws, rest) = T.span isSpace txt
          col = sourceColumn pos + T.length eaten
          newSourcePos = T.foldl' f (newPos "sourceName" (sourceLine pos) col) ws
          f pos' '\n' = setSourceColumn (incSourceLine pos' 1) 0
          f pos' '\r' = pos'
          f pos' _    = incSourceColumn pos' 1
       in (rest, newSourcePos)
