{
module Lexer (
  Token(..),
  scanTokens
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
$string_lit = (?>"(?>\\(?>["\\\/bfnrt]|u[a-fA-F0-9]{4})|[^"\\\0-\x1F\x7F]+)*")
$num_lit = -?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?
$ident = [a-zA-Z]+[a-zA-Z0-9'\_]*

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  $ident                        { \s -> Identifier (T.pack s)}
  $string_lit                   { \s -> StringLit (T.pack s)}
  $num_lit                      { \s -> NumLit (read s) }
  if                            { \s -> Identifier "if" }
  else                          { \s -> Identifier "else" }
  end                           { \s -> Identifier "end" }
  null                          { \s -> Identifier "null" }
  range                         { \s -> Identifier "range" }
  escapeUri                     { \s -> Identifier "escapeUri" }
  true                          { \s -> Identifier (T.pack "true") }
  false                         { \s -> Identifier (T.pack "false") }

  \'                            { \s -> SingleQuote }
  \:                            { \s -> Colon }
  \.                            { \s -> Dot }
  \,                            { \s -> Comma }
  \==                           { \s -> Eq }
  \>                            { \s -> Gt }
  \<                            { \s -> Lt }
  \<                            { \s -> Lt }
  \&\&                          { \s -> And }
  \|\|                          { \s -> Or }
  \_                            { \s -> Underscore }
  \:\=                          { \s -> Assignment}

  \{                            { \s -> CurlyOpen }
  \}                            { \s -> CurlyClose }
  \{\{                          { \s -> DoubleCurlyOpen }
  \}\}                          { \s -> DoubleCurlyClose }
  \[                            { \s -> SquareOpen }
  \]                            { \s -> SquareClose }
  \(                            { \s -> ParenOpen }
  \)                            { \s -> ParenClose }

{
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
  | DoubleCurlyOpen
  | DoubleCurlyClose
  | SquareOpen
  | SquareClose
  | ParenOpen
  | ParenClose
  | Underscore
  | Assignment
  deriving (Show, Eq, Ord, Generic)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
