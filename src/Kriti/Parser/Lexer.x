{
module Kriti.Parser.Lexer where

import qualified Data.Text as T
import Kriti.Parser.Monad
import Kriti.Parser.Spans
import Kriti.Parser.Token
}

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z09]

tokens :-

-- Whitespace insensitive
<0, template, braces> $white+                       ;

-- Comments
<0> "#".*                         ;

-- Syntax
<0> range                                                               { token TokIdentifier }
<0, template, braces> true                                              { token (TokBoolLit . (\(Loc sp _) -> Loc sp True)) }
<0, template, braces> false                                             { token (TokBoolLit . (\(Loc sp _) -> Loc sp False)) }
<0, template, braces> \$? $alpha [\$ $alpha $digit \_ \-]*              { token TokIdentifier }

-- Enter String Mode
<0> \" { \b -> pushStartCode string *> token (\(Loc sp _) -> StringBegin sp) b }

-- Produce a String Literal Value
<string> (\\ \\ | \\ \` | [^ \" \{ ])+ { token TokStringLit }

-- Enter Template Mode if we consume `{{`
<string> \{ \{ { \b -> pushStartCode template *> token (\(Loc sp _) -> TemplateBegin sp) b }

-- All standard non-StringLit rules can apply in this status mode.

-- Enter Braces Mode to avoid terminating the template
<template> \{ \{ { \b -> (pushStartCode braces *> symbol SymDoubleCurlyOpen b) }

-- Exit Braces Mode to allow terminating the template
<braces> \} \} { \b -> (popStartCode *> symbol SymDoubleCurlyClose b) } 

-- Exit Template mode if we consume `}}`
<template> \} \} { \b -> popStartCode *> token (\(Loc sp _) -> TemplateEnd sp) b }

-- Exit String Mode
<string> \" { \b -> popStartCode *> token (\(Loc sp _) -> StringEnd sp) b }


-- \"([^\\\"]+)\"                                    { mkTemplate }


<0, template, braces> \-? $digit+                                       { token (\loc -> TokIntLit (unlocate loc) (read . T.unpack <$> loc)) }
<0, template, braces> \-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)? { token (\loc -> TokNumLit (unlocate loc) (read . T.unpack <$> loc)) }
<0, template, braces> \'                                                { symbol SymSingleQuote }
<0, template, braces> \:                                                { symbol SymColon }
<0, template, braces> \.                                                { symbol SymDot }
<0, template, braces> \,                                                { symbol SymComma }
<0, template, braces> \==                                               { symbol SymEq }
<0, template, braces> \>                                                { symbol SymGt }
<0, template, braces> \<                                                { symbol SymLt }
<0, template, braces> \<                                                { symbol SymLt }
<0, template, braces> \&\&                                              { symbol SymAnd }
<0, template, braces> \|\|                                              { symbol SymOr }
<0, template, braces> \_                                                { symbol SymUnderscore }
<0, template, braces> \:\=                                              { symbol SymAssignment }

<0> \{                                                                  { symbol SymCurlyOpen }
<0> \}                                                                  { symbol SymCurlyClose }
<0, template, braces> \{\{                                              { symbol SymDoubleCurlyOpen }
<0, template, braces> \}\}                                              { symbol SymDoubleCurlyClose }
<0, template, braces> \[                                                { symbol SymSquareOpen }
<0, template, braces> \]                                                { symbol SymSquareClose }
<0, template, braces> \(                                                { symbol SymParenOpen }
<0, template, braces> \)                                                { symbol SymParenClose }

{
scan :: Parser Token
scan = do
  input <- getInput
  code <- startCode
  case alexScan input code of
    AlexEOF -> pure EOF
    AlexError (AlexInput pos _ _ _) ->
      parseError $ InvalidLexeme pos
    AlexSkip rest len -> do
      advance rest
      scan
    AlexToken rest nbytes action -> do
      advance rest
      action (slice nbytes input)

lexer :: Parser [Token]
lexer = do
  tok <- scan
  case tok of
    EOF -> pure []
    x -> (x :) <$> lexer
}
