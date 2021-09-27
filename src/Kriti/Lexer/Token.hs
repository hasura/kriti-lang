module Kriti.Lexer.Token where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Scientific (Scientific)
import qualified Data.Text as T
import GHC.Generics
import qualified Text.Megaparsec as P

viewSC :: P.SourcePos -> P.Pos
viewSC = P.sourceColumn

setSC :: P.Pos -> P.SourcePos -> P.SourcePos
setSC i pos = pos {P.sourceColumn = i}

overSC :: (P.Pos -> P.Pos) -> P.SourcePos -> P.SourcePos
overSC f pos = pos {P.sourceColumn = f (viewSC pos)}

incSC :: Int -> P.SourcePos -> P.SourcePos
incSC i = overSC (<> P.mkPos i)

viewSL :: P.SourcePos -> P.Pos
viewSL = P.sourceLine

setSL :: P.Pos -> P.SourcePos -> P.SourcePos
setSL i pos = pos {P.sourceLine = i}

overSL :: (P.Pos -> P.Pos) -> P.SourcePos -> P.SourcePos
overSL f pos = pos {P.sourceLine = f (viewSL pos)}

incSL :: Int -> P.SourcePos -> P.SourcePos
incSL i = overSL (<> P.mkPos i)

class Serialize t where
  serialize :: t -> T.Text

data Token
  = -- | String Template
    StringTem T.Text
  | -- | Identifier
    Identifier T.Text
  | -- | Number literal with original string
    NumLit T.Text Scientific
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
  | -- | Member
    CurlyOpen
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
    StringTem str -> "\"" <> str <> "\""
    Identifier iden -> iden
    NumLit str _ -> str
    BoolLit True -> "true"
    BoolLit False -> "false"
    Bling -> "$"
    Colon -> ":"
    Dot -> "."
    Comma -> ","
    Eq -> "=="
    Gt -> ">"
    Lt -> "<"
    And -> "&&"
    Or -> "||"
    CurlyOpen -> "{"
    CurlyClose -> "}"
    SquareOpen -> "["
    SquareClose -> "]"
    ParenOpen -> "("
    ParenClose -> ")"
    Underscore -> "_"
    Assignment -> ":="

data TokenExt = TokenExt {teType :: Token, teStartPos :: P.SourcePos, teEndPos :: P.SourcePos, teLength :: Int}
  deriving (Show, Eq, Ord)

data TokenStream = TokenStream {tsStreamInput :: String, tsTokens :: [TokenExt]}
  deriving (Show, Eq, Ord)

-- | Extract a single line of tokens from the stream
viewLine :: P.Pos -> TokenStream -> [TokenExt]
viewLine n (TokenStream _ toks) = filter (\TokenExt {teStartPos} -> n == viewSL teStartPos) toks

instance Serialize TokenStream where
  serialize (TokenStream _ []) = mempty
  serialize (TokenStream _ toks) =
    let colDiff p1 p2 = P.unPos (viewSC p2) - P.unPos (viewSC p1)
        lineDiff p1 p2 = P.unPos (viewSL p2) - P.unPos (viewSL p1)
        f :: (T.Text, P.SourcePos) -> TokenExt -> (T.Text, P.SourcePos)
        f (txt, pos) (TokenExt tok nextPos _ _) =
          if viewSL pos == viewSL nextPos
            then
              ( txt <> T.replicate (colDiff pos nextPos) " " <> serialize tok,
                incSC (T.length $ serialize tok) nextPos
              )
            else
              ( mconcat
                  [ txt,
                    T.replicate (lineDiff pos nextPos) "\n",
                    T.replicate (P.unPos (viewSC nextPos) - 1) " ",
                    serialize tok
                  ],
                incSC (T.length $ serialize tok) nextPos
              )
     in fst $ foldl f (mempty, P.initialPos mempty) toks

instance Serialize [TokenExt] where
  serialize = serialize . TokenStream mempty

instance P.Stream TokenStream where
  type Token TokenStream = TokenExt
  type Tokens TokenStream = [TokenExt]

  tokenToChunk pxy = P.tokensToChunk pxy . pure
  tokensToChunk _ = id

  chunkToTokens _ toks = toks
  chunkLength pxy = length . P.chunkToTokens pxy

  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream si (tok : toks)) =
    Just (tok, TokenStream (drop (P.tokensLength (Proxy @TokenStream) (tok :| [])) si) toks)

  takeN_ i (TokenStream si toks)
    | i <= 0 = Just ([], TokenStream si toks)
    | null toks = Nothing
    | otherwise =
      let (prev, rest) = splitAt i toks
       in case NE.nonEmpty prev of
            Nothing -> Just (prev, TokenStream si rest)
            Just next -> Just (prev, TokenStream (drop (P.tokensLength (Proxy @TokenStream) next) si) rest)

  takeWhile_ f (TokenStream si toks) =
    --TokenStream si <$> span f toks
    let (prev, rest) = span f toks
     in case NE.nonEmpty prev of
          Nothing -> (prev, TokenStream si rest)
          Just next -> (prev, TokenStream (drop (P.tokensLength (Proxy @TokenStream) next) si) rest)

instance P.TraversableStream TokenStream where
  reachOffset o P.PosState {..} =
    let (pre, post) = splitAt (o - pstateOffset) (tsTokens pstateInput)
        tokensConsumed =
          case NE.nonEmpty pre of
            Nothing -> 0
            Just nePre -> P.tokensLength (Proxy @TokenStream) nePre
        (preStr, postStr) = splitAt tokensConsumed (tsStreamInput pstateInput)
        preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
        restOfLine = takeWhile (/= '\n') postStr
        newSourcePos =
          case post of
            [] -> pstateSourcePos
            (x : _) -> teStartPos x
        sameLine = P.sourceLine newSourcePos == P.sourceLine pstateSourcePos
        prefix =
          if sameLine
            then pstateLinePrefix ++ preLine
            else preLine
     in ( Just $ prefix <> restOfLine,
          P.PosState
            { P.pstateInput = TokenStream postStr post,
              P.pstateOffset = max pstateOffset o,
              P.pstateSourcePos = newSourcePos,
              P.pstateTabWidth = pstateTabWidth,
              P.pstateLinePrefix = prefix
            }
        )

instance P.VisualStream TokenStream where
  showTokens Proxy = unwords . NE.toList . fmap (show . serialize . teType)
