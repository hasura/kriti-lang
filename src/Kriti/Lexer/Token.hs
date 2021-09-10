{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Kriti.Lexer.Token where

import Data.Scientific (Scientific)
import GHC.Generics

import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec    as P
import qualified Data.Text          as T

viewSC :: P.SourcePos -> P.Pos
viewSC = P.sourceColumn

setSC :: P.Pos -> P.SourcePos -> P.SourcePos
setSC i pos = pos { P.sourceColumn = i }

overSC :: (P.Pos -> P.Pos) -> P.SourcePos -> P.SourcePos
overSC f pos = pos { P.sourceColumn = f (viewSC pos) }

incSC :: Int -> P.SourcePos -> P.SourcePos
incSC i = overSC (<> P.mkPos i)

viewSL :: P.SourcePos -> P.Pos
viewSL = P.sourceLine

setSL :: P.Pos -> P.SourcePos -> P.SourcePos
setSL i pos = pos { P.sourceLine = i }

overSL :: (P.Pos -> P.Pos) -> P.SourcePos -> P.SourcePos
overSL f pos = pos { P.sourceLine = f (viewSL pos) }

incSL :: Int -> P.SourcePos -> P.SourcePos
incSL i = overSL (<> P.mkPos i)

class Serialize t where
  serialize :: t -> T.Text

data Token =
    StringLit T.Text
    -- ^ String Literal
  | Identifier T.Text
    -- ^ Identifier
  | NumLit T.Text Scientific
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
  deriving (Show, Eq, Ord, Generic)

instance Serialize Token where
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

data TokenExt = TokenExt { teType :: Token, tePos :: P.SourcePos }
  deriving (Show, Eq, Ord)

newtype TokenStream = TokenStream { getTokens :: [TokenExt] }
  deriving (Show, Eq, Ord)

-- | Extract a single line of tokens from the stream
viewLine :: P.Pos -> TokenStream -> [TokenExt]
viewLine n (TokenStream toks) = filter (\TokenExt{tePos} -> n == viewSL tePos) toks

instance Serialize TokenStream where
  serialize (TokenStream []) = mempty
  serialize (TokenStream toks) =
    let colDiff p1 p2 = P.unPos (viewSC p2) - P.unPos (viewSC p1)
        lineDiff p1 p2 = P.unPos (viewSL p2) - P.unPos (viewSL p1)
        f :: (T.Text, P.SourcePos) -> TokenExt -> (T.Text, P.SourcePos)
        f (txt, pos) (TokenExt tok nextPos) =
          if viewSL pos == viewSL nextPos
          then (txt <> T.replicate (colDiff pos nextPos) " " <> serialize tok
               , incSC (T.length $ serialize tok) nextPos)
          else (mconcat
                  [ txt
                  , T.replicate (lineDiff pos nextPos) "\n"
                  , T.replicate (P.unPos (viewSC nextPos) - 1) " "
                  ,  serialize tok
                  ]
               , incSC (T.length $ serialize tok) nextPos)
    in fst $ foldl f (mempty, P.initialPos mempty) toks

instance Serialize [TokenExt] where
  serialize = serialize . TokenStream

instance P.Stream TokenStream where
  type Token TokenStream = TokenExt
  type Tokens TokenStream = [TokenExt]

  tokenToChunk pxy = P.tokensToChunk pxy . pure
  tokensToChunk _ = id

  chunkToTokens _ toks = toks
  chunkLength pxy = length . P.chunkToTokens pxy

  take1_ (TokenStream []) = Nothing
  take1_ (TokenStream (tok : toks)) = Just (tok, TokenStream toks)

  takeN_ i (TokenStream toks)
    | i <= 0           = Just ([], TokenStream toks)
    | null toks = Nothing
    | length toks <= i = Just (toks, TokenStream [])
    | otherwise        = Just (take i toks, TokenStream $ drop 1 toks)

  takeWhile_ f (TokenStream toks) = TokenStream <$> span f toks

instance P.TraversableStream TokenStream where
  reachOffset o P.PosState{..} =
    let (_, post) = splitAt (o - pstateOffset) (getTokens pstateInput)
        spos = if null post
          then pstateSourcePos
          else tePos $ head post
        addPrefix xs =
          if viewSL spos == viewSL pstateSourcePos
          then pstateLinePrefix ++ xs
          else xs
    in ( Just $ addPrefix $ T.unpack $ serialize $ viewLine (viewSL spos) pstateInput
       , P.PosState
           { P.pstateInput = TokenStream post
           , P.pstateOffset = max pstateOffset o
           , P.pstateSourcePos = spos
           , P.pstateTabWidth = pstateTabWidth
           , P.pstateLinePrefix = mempty
           }
       )

instance P.VisualStream TokenStream where
  showTokens _ = T.unpack . serialize . TokenStream . NE.toList
