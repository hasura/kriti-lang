module Kriti.Lexer where

import           Kriti.Error

import           Control.Monad.Except         (MonadError, throwError)
import           Data.Char                    (isAlpha, isSpace)
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
import Text.ParserCombinators.Parsec.Expr (Operator(Postfix))

data Token =
    StringLit Text
    -- ^ String Literal
  | Identifier Text
    -- ^ Identifier
  | NumLit Text Scientific
    -- ^ Number literal with original string
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
    NumLit str _    -> str
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

newtype LexError = LexError { lePos :: SourcePos }
  deriving Show

instance RenderError LexError where
  render LexError{lePos} =
    RenderedError
      { _code = LexErrorCode
      , _message = "Invalid Lexeme"
      , _span = (fromSourcePos lePos, Nothing)
      }

throwLexError :: MonadError LexError m => SourcePos -> m a
throwLexError = throwError . LexError

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f = go
  where
    go b = f b >>= \case
      Just (a, b') -> do
        as <- go b'
        pure $ a:as
      Nothing -> pure []
{-# inlineable unfoldrM #-}
{-# LANGUAGE FlexibleContexts #-}

lexer :: Text -> Either LexError [TokenExt]
lexer t = do
  (t', iPos) <- init t (initialPos "sourceName") mempty
  unfoldrM go (t', iPos)
  where
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
      | Just (str, _, s) <- stringLit   txt = stepLexer (StringLit str) s pos
      | Just (str, _, s) <- identifier  txt = stepLexer (Identifier str) s pos
      | Just (n, matched, s)   <- numberLit   txt = stepLexer (NumLit matched (realToFrac n)) s pos
      | otherwise = pure Nothing

    stepLexer :: Token -> Text -> SourcePos -> Either LexError (Maybe (TokenExt, (Text, SourcePos)))
    stepLexer tok rest pos = do
      str <- advance rest pos (serialize tok)
      pure $ Just (TokenExt tok pos, str)

    identifier :: Text -> Maybe (Text, Text, Text) -- (value, lit, remainder)
    identifier = fromRead (readPrec_to_P identLexeme 0)
      where
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

    init :: Text -> SourcePos -> Text -> Either LexError (Text, SourcePos)
    init txt pos eaten =
      let (ws, rest) = T.span isSpace txt
          col = sourceColumn pos + T.length eaten
          newSourcePos = T.foldl' f (newPos "sourceName" (sourceLine pos) col) ws
          f pos' '\n' = setSourceColumn (incSourceLine pos' 1) 0
          f pos' '\r' = pos'
          f pos' _    = incSourceColumn pos' 1
      in pure (rest, newSourcePos)

    advance :: Text -> SourcePos -> Text -> Either LexError (Text, SourcePos)
    advance txt pos eaten =
      case T.span isSpace txt of
       --("", rest) | not (T.null rest) && isAlpha (T.head rest) -> throwLexError pos
       (ws, rest) ->
         let col = sourceColumn pos + T.length eaten
             newSourcePos = T.foldl' f (newPos "sourceName" (sourceLine pos) col) ws
             f pos' '\n' = setSourceColumn (incSourceLine pos' 1) 0
             f pos' '\r' = pos'
             f pos' _    = incSourceColumn pos' 1
         in pure (rest, newSourcePos)
