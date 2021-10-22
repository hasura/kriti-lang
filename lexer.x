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
$eol   = [\n]


tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  true                          { \s -> Identifier "true" }
  false                         { \s -> Identifier "false" }
  $digit+                       { \s -> TokenNum (read s) }
  "->"                          { \s -> TokenArrow }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  [\+]                          { \s -> TokenAdd }
  [\-]                          { \s -> TokenSub }
  [\*]                          { \s -> TokenMul }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

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
