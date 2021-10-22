{
module Kriti.Lexer (
  Token(..),
  TokenExt(..),
  lexer,
  serialize
) where

import Data.Scientific (Scientific)
import qualified Data.Text as T
import GHC.Generics
import qualified Kriti.Error as E

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
  deriving (Show, Eq, Ord, Generic)

class Serialize t where
  serialize :: t -> T.Text

instance Serialize Token where
  serialize = \case
    StringTem str -> "\"" <> str <> "\""
    --StringLit str -> "\'" <> str <> "\'"
    Identifier iden -> iden
    IntLit str _ -> str
    NumLit str _ -> str
    BoolLit True -> "true"
    BoolLit False -> "false"
    Bling -> "$"
    Colon -> ":"
    Dot -> "."
    Comma -> ","
    SingleQuote -> "'"
    DoubleCurlyOpen -> "{{"
    DoubleCurlyClose -> "}}"
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

data TokenExt = TokenExt {teType :: Token, teStartPos :: E.SourcePosition, teEndPos :: E.SourcePosition, teLength :: Int}
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
      start = E.SourcePosition "" l c
      end = E.SourcePosition "" l (c + len - 1)
  in TokenExt tok start end len

lexer :: String -> [TokenExt]
lexer = alexScanTokens
}
