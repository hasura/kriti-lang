{
module Kriti.Lexer (
  Token(..),
  TokenExt(..),
  lexer
) where

import Control.Exception (Exception, throw)
import Control.Monad.State
import Data.Scientific (Scientific)
import qualified Data.Text as T
import GHC.Generics

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z09]
$eol = [\n]
-- $string_lit = (?>"(?>\\(?>["\\\/bfnrt]|u[a-fA-F0-9]{4})|[^"\\\0-\x1F\x7F]+)*")
$num_lit = $alpha

tokens :-

-- Whitespace insensitive
$white+                       ;

-- Comments
"#".*                         ;

-- Syntax
if                                                { mkTok (const $ Identifier "if") }
else                                              { mkTok (const $ Identifier "else") }
end                                               { mkTok (const $ Identifier "end") }
null                                              { mkTok (const $ Identifier "null") }
range                                             { mkTok (const $ Identifier "range") }
escapeUri                                         { mkTok (const $ Identifier "escapeUri") }
true                                              { mkTok (const $ BoolLit True) }
false                                             { mkTok (const $ BoolLit False) }
\$? $alpha [$alpha $digit \_ \-]*                 { mkTok (Identifier . T.pack)}
\"([^\\\"]+)\"                                    { mkTok (StringTem . unwrap . T.pack) }
\-? $digit+                                       { mkTok (\s -> IntLit (T.pack s) (read s)) }
\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)? { mkTok (\s -> NumLit (T.pack s) (read s)) }

\'                                       { mkTok (const $ SingleQuote) }
\:                                       { mkTok (const $ Colon) }
\.                                       { mkTok (const $ Dot) }
\,                                       { mkTok (const $ Comma) }
\==                                      { mkTok (const $ Eq) }
\>                                       { mkTok (const $ Gt) }
\<                                       { mkTok (const $ Lt) }
\<                                       { mkTok (const $ Lt) }
\&\&                                     { mkTok (const $ And) }
\|\|                                     { mkTok (const $ Or) }
\_                                       { mkTok (const $ Underscore) }
\:\=                                     { mkTok (const $ Assignment)}

\{                                       { mkTok (const $ CurlyOpen) }
\}                                       { mkTok (const $ CurlyClose) }
\{\{                                     { mkTok (const $ DoubleCurlyOpen) }
\}\}                                     { mkTok (const $ DoubleCurlyClose) }
\[                                       { mkTok (const $ SquareOpen) }
\]                                       { mkTok (const $ SquareClose) }
\(                                       { mkTok (const $ ParenOpen) }
\)                                       { mkTok (const $ ParenClose) }

{
data Token
  = -- | String Template
    StringTem T.Text
  | -- | Identifier
    Identifier T.Text
  | -- | Number literal with original string
    NumLit T.Text Scientific
  | IntLit T.Text Int
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
  | SingleQuote
  | CurlyOpen
  | CurlyClose
  | DoubleCurlyOpen
  | DoubleCurlyClose
  | SquareOpen
  | SquareClose
  | ParenOpen
  | ParenClose
  | Underscore
  | Assignment
  | EOF
  deriving (Show, Eq, Ord, Generic)

newtype InvalidPosException = InvalidPosException Int
  deriving Show

instance Exception InvalidPosException

newtype Pos = Pos { unPos :: Int }
  deriving (Show, Eq, Ord)

instance Semigroup Pos where
  Pos i <> Pos j = Pos (i + j)

mkPos :: Int -> Pos
mkPos i =
  if i <= 0
    then throw (InvalidPosException i)
    else Pos i

pos1 :: Pos
pos1 = Pos 1

data SourcePos = SourcePos { sourceLine :: Pos, sourceColumn :: Pos }
  deriving (Show, Eq, Ord)

initialSourcePos :: SourcePos
initialSourcePos = SourcePos pos1 pos1

data TokenExt = TokenExt {teType :: Token, teStartPos :: SourcePos, teEndPos :: SourcePos, teLength :: Int}
  deriving (Show, Eq, Ord)

unwrap :: T.Text -> T.Text
unwrap txt =
  case T.uncons txt of
    Just ('"', txt') ->
      case T.unsnoc txt' of
        Just (txt'', '"') -> txt''
        _ -> txt'
    _ -> txt

mkTok :: (String -> Token) -> AlexPosn -> String -> TokenExt
mkTok f (AlexPn _ l c) s =
  let tok = f s
      len = length s
      start = SourcePos (mkPos l) (mkPos c)
      end = SourcePos (mkPos l) (mkPos (c + len))
  in TokenExt tok start end len

lexer :: String -> [TokenExt]
lexer = alexScanTokens

--lexer = alexMonadScan
--alexEOF = pure EOF
}
