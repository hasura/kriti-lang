{
module Kriti.Lexer (
  Token(..),
  lexer
) where

import Data.Scientific (Scientific)
import qualified Data.Text as T
import GHC.Generics

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z09]
$eol   = [\n]
-- $string_lit = (?>"(?>\\(?>["\\\/bfnrt]|u[a-fA-F0-9]{4})|[^"\\\0-\x1F\x7F]+)*")
$num_lit = $alpha

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  if                                                { \_ -> Identifier "if" }
  else                                              { \_ -> Identifier "else" }
  end                                               { \_ -> Identifier "end" }
  null                                              { \_ -> Identifier "null" }
  range                                             { \_ -> Identifier "range" }
  escapeUri                                         { \_ -> Identifier "escapeUri" }
  true                                              { \_ -> BoolLit True }
  false                                             { \_ -> BoolLit False }
  \$? $alpha [$alpha $digit \_ \-]*                 { \s -> Identifier (T.pack s)}
  \"([^\\\"]+)\"                                    { \s -> StringTem (unwrap $ T.pack s) }
  \-? $digit+                                       { \s -> IntLit (T.pack s) (read s) }
  \-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)? { \s -> NumLit (T.pack s) (read s) }

  \'                                       { \_ -> SingleQuote }
  \:                                       { \_ -> Colon }
  \.                                       { \_ -> Dot }
  \,                                       { \_ -> Comma }
  \==                                      { \_ -> Eq }
  \>                                       { \_ -> Gt }
  \<                                       { \_ -> Lt }
  \<                                       { \_ -> Lt }
  \&\&                                     { \_ -> And }
  \|\|                                     { \_ -> Or }
  \_                                       { \_ -> Underscore }
  \:\=                                     { \_ -> Assignment}

  \{                                       { \_ -> CurlyOpen }
  \}                                       { \_ -> CurlyClose }
  \{\{                                     { \_ -> DoubleCurlyOpen }
  \}\}                                     { \_ -> DoubleCurlyClose }
  \[                                       { \_ -> SquareOpen }
  \]                                       { \_ -> SquareClose }
  \(                                       { \_ -> ParenOpen }
  \)                                       { \_ -> ParenClose }

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

unwrap :: T.Text -> T.Text
unwrap txt =
  case T.uncons txt of
    Just ('"', txt') ->
      case T.unsnoc txt' of
        Just (txt'', '"') -> txt''
        _ -> txt'
    _ -> txt

lexer :: String -> [Token]
lexer = alexScanTokens

}
