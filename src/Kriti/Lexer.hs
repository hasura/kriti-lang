module Kriti.Lexer where

import           Kriti.Error
import           Kriti.Lexer.Token

import           Control.Monad.Except         (MonadError, throwError)
import           Data.Char                    (isSpace)
import           Data.Maybe                   (maybeToList)
import           Data.Scientific              (Scientific, scientificP)
import           Data.Text                    (Text)
import           Text.ParserCombinators.ReadP (ReadP, gather, readP_to_S)
import           Text.Read                    (lexP, lift, readPrec_to_P)
import           Text.Read.Lex.Extended       (lexTemplate)

import qualified Data.Text                    as T
import qualified Text.Megaparsec              as P
import qualified Text.Read.Lex                as L

newtype LexError = LexError { lePos :: P.SourcePos }
  deriving (Show, Eq, Ord)

instance P.ShowErrorComponent LexError where
  showErrorComponent = show

instance RenderError LexError where
  render LexError{lePos} =
    RenderedError
      { _code = LexErrorCode
      , _message = "Invalid Lexeme"
      , _span = (fromSourcePos lePos, Nothing)
      }

throwLexError :: MonadError LexError m => P.SourcePos -> m a
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

lexer :: Text -> Either LexError TokenStream
lexer t = do
  (t', iPos) <- initialize t (P.initialPos "sourceName") mempty
  TokenStream mempty <$> unfoldrM go (t', iPos)
  where
    go :: (Text, P.SourcePos) -> Either LexError (Maybe (TokenExt, (Text, P.SourcePos)))
    go (txt, pos)
      | T.null txt = pure Nothing
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
      --  | Just (str, _, s) <- stringLit   txt = stepLexer (StringLit str) s pos
      | Just (str, _, s) <- stringTem   txt = stepLexer (StringTem str) s pos
      | Just (str, _, s) <- identifier  txt = stepLexer (Identifier str) s pos
      | Just (n, matched, s) <- numberLit txt = stepLexer (NumLit matched (realToFrac n)) s pos
      | otherwise = throwLexError pos

    stepLexer :: Token -> Text -> P.SourcePos -> Either LexError (Maybe (TokenExt, (Text, P.SourcePos)))
    stepLexer tok rest pos = do
      let serialized = serialize tok
          tokLength = T.length serialized
          endPos = incSC tokLength pos
      str <- advance rest pos serialized
      pure $ Just (TokenExt tok pos endPos tokLength, str)

    identifier :: Text -> Maybe (Text, Text, Text) -- (value, lit, remainder)
    identifier = fromRead (readPrec_to_P identLexeme 0)
      where
        identLexeme = do
          L.Ident s <- lexP
          pure (T.pack s)

    --stringLit :: Text -> Maybe (Text, Text, Text) -- (value, lit, remainder)
    --stringLit = fromRead (readPrec_to_P stringLexeme 0)
    --  where
    --    stringLexeme = do
    --      L.String s <- lexP
    --      pure (T.pack s)

    stringTem :: Text -> Maybe (Text, Text, Text) -- (value, lit, remainder)
    stringTem = fromRead (readPrec_to_P stringLexeme 0)
      where
        stringLexeme = do
          L.String s <- lift lexTemplate
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

    initialize :: Text -> P.SourcePos -> Text -> Either LexError (Text, P.SourcePos)
    initialize txt pos eaten =
      let (ws, rest) = T.span isSpace txt
          col = if T.length eaten == 0 then P.sourceColumn pos else P.sourceColumn pos <> P.mkPos (T.length eaten)
          newSourcePos = T.foldl' f (P.SourcePos "sourceName" (P.sourceLine pos) col) ws
          f :: P.SourcePos -> Char -> P.SourcePos
          f pos' '\n' = setSC (P.mkPos 1) $ incSL 1 pos'
          f pos' '\r' = pos'
          f pos' _    = incSC 1 pos'
      in pure (rest, newSourcePos)

    advance :: Text -> P.SourcePos -> Text -> Either LexError (Text, P.SourcePos)
    advance txt pos eaten =
      case T.span isSpace txt of
       --("", rest) | not (T.null rest) && isAlpha (T.head rest) -> throwLexError pos
       (ws, rest) ->
         let col = viewSC pos <> P.mkPos (T.length eaten)
             newSourcePos = T.foldl' f (P.SourcePos "sourceName" (viewSL pos) col) ws
             f pos' '\n' = setSC (P.mkPos 1) (incSL 1 pos')
             f pos' '\r' = pos'
             f pos' _    = incSC 1 pos'
         in pure (rest, newSourcePos)
