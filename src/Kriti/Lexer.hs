module Kriti.Lexer where

import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Maybe (maybeToList)
import Data.Scientific (Scientific, scientificP)
import Data.Text (Text)
import Text.ParserCombinators.ReadP (ReadP, gather, readP_to_S)
import Text.Read (lexP, lift, readPrec_to_P)

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Read.Lex as L
import           Text.Read.Lex.Extended (lexString)

import Kriti.Lexer.Token

lexer :: Text -> TokenStream
lexer t = TokenStream $ unfoldr go (t', iPos)
  where
    (t', iPos) = advance t (P.initialPos "sourceName") mempty
    go :: (Text, P.SourcePos) -> Maybe (TokenExt, (Text, P.SourcePos))
    go (txt, pos)
      | T.null t = Nothing
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
      | Just (str, matched, s) <- stringLit txt  = Just (TokenExt (StringLit str) pos, advance s pos matched)
      | Just (str, matched, s) <- identifier txt = Just (TokenExt (Identifier str) pos, advance s pos matched)
      | Just (n, matched, s) <- numberLit txt    = Just (TokenExt (NumLit (realToFrac n)) pos, advance s pos matched)
      | otherwise = Nothing

    stepLexer :: Token -> Text -> P.SourcePos -> Maybe (TokenExt, (Text, P.SourcePos))
    stepLexer tok s pos = Just (TokenExt tok pos, advance s pos (serialize tok))

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

    advance :: Text -> P.SourcePos -> Text -> (Text, P.SourcePos)
    advance txt pos eaten =
      let (ws, rest) = T.span isSpace txt
          col = if T.length eaten == 0 then P.sourceColumn pos else P.sourceColumn pos <> P.mkPos (T.length eaten)
          newSourcePos = T.foldl' f (P.SourcePos "sourceName" (P.sourceLine pos) col) ws
          f :: P.SourcePos -> Char -> P.SourcePos
          f pos' '\n' = setSC (P.mkPos 1) $ incSL 1 pos'
          f pos' '\r' = pos'
          f pos' _ = incSC 1 pos'
       in (rest, newSourcePos)
