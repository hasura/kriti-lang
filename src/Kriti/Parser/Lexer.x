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
<0, expr> $white+                 ;

-- Comments
<0, expr> "#".*                   ;

-- Syntax
<0> range                                      { token TokIdentifier }
<0, expr> true                                 { token (TokBoolLit . (\(Loc sp _) -> Loc sp True)) }
<0, expr> false                                { token (TokBoolLit . (\(Loc sp _) -> Loc sp False)) }
<0, expr> \$                                   { token TokIdentifier }
<0, expr> \$? $alpha [\$ $alpha $digit \_ \-]* { token TokIdentifier }

-- | String Templating
--
-- String Templates are represented in our AST as a `StringTem Span
-- (Vector ValueExt)`. In our lexeme stream we represent this as sequence of tokens:
--
-- "www.google.com/{{$path}}" => [TokSymbol SymStringBegin, TokStringLit (Loc _ "www.google.com/"), TokSymbol SymDoubleCurlyOpen _, TokIdentifier (Loc _ "$path"), TokSymbol SymDoubleCurlyClose _, TokSymbol SymStringEnd _]
--
-- Everything within the `SymStringBegin` and `SymStringEnd` tokens is
-- part of the same String Template. Template fragments are wrapped in
-- `SymDoubleCurlyOpen` and `SymDoubleCurlyClose` tokens.
--
-- This representation can be achieved in a single pass of Alex with a
-- second cleanup pass to concat adjacent `TokStringLit` tokens and is
-- easy to parse with Happy.
--
-- To achieve this we must use two Alex Status Codes: <string> and <expr>.
--
-- The core idea is to shift the lexer into <string> when we encounter
-- '\"' and <expr> when we encounter '{{'. We exit out of <expr> into
-- <string> when we encounter '}}' and out of <string> into <0> when
-- we encounter '\"'.

-- Enter String Mode
<0, expr> \" { \b -> pushStartCode string *> symbol SymStringBegin b}

-- In <string> we have three rules:
-- 1. Capture a string literal
<string> (\\ \\ | \\ \` | [^ \" \{ ])+ { token TokStringLit }
-- 2. Capture a '{' as a string literal
<string> \{ { token TokStringLit}
-- 3. Capture '{{', enter <expr> mode, and emit a 'ExprBegin' token. This will win over the '{' rule due to the longest capture rule.
<string> \{ \{ { \b -> (pushStartCode expr *> symbol SymDoubleCurlyOpen b) }

-- Note: We need rule 2 to allow for string literals such as `"foo{"`.
-- This also means that '{' will be in its own `TokStringLit` required
-- a post lexing pass to concatenate adjacent `TokStringLit` tokens.

-- In <expr> mode we allow the subset of <0> mode rules required to
-- construct our allowed expressions: Bools, Numbers, and Paths.

-- Exit <expr> mode if we match '}}'
<expr> \} \} { \b -> (popStartCode *> symbol SymDoubleCurlyClose b) }

-- Exit <string> Mode if we match '\"'
<string> \" { \b -> (popStartCode *> symbol SymStringEnd b) }

-- String literals can be constructed with single quotes. These are
-- only used for object lookup and are thus only available in <expr>
-- mode.
<expr> \'                              { \b -> pushStartCode literal *> symbol SymSingleQuote b }
<literal> (\\ \\ | \\ \` | [^ \' \{ ])+ { token TokStringLit }
<literal> \'                            { \b -> (popStartCode *> symbol SymSingleQuote b) }

<0, expr> \-? $digit+                                       { token (\loc -> TokIntLit (unLoc loc) (read . T.unpack <$> loc)) }
<0, expr> \-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)? { token (\loc -> TokNumLit (unLoc loc) (read . T.unpack <$> loc)) }
<0> \:                                                      { symbol SymColon }
<0, expr> \.                                                { symbol SymDot }
<0, expr> \,                                                { symbol SymComma }
<0, expr> \==                                               { symbol SymEq }
<0, expr> \>                                                { symbol SymGt }
<0, expr> \<                                                { symbol SymLt }
<0, expr> \<                                                { symbol SymLt }
<0, expr> \&\&                                              { symbol SymAnd }
<0, expr> \|\|                                              { symbol SymOr }
<0, expr> \_                                                { symbol SymUnderscore }
<0, expr> \:\=                                              { symbol SymAssignment }
<0, expr> \{                                                { symbol SymCurlyOpen }
<0, expr> \}                                                { symbol SymCurlyClose }

-- When we encounter a `{{` we push an <expr> code. This way we know
-- when to parse two `}` as `SymDoubleCurlyClose` rather then as two
-- `SymCurlyClose`
<0, expr> \{ \{                                             { \b -> (pushStartCode expr *> symbol SymDoubleCurlyOpen b) }

<0, expr> \[                                                { symbol SymSquareOpen }
<0, expr> \]                                                { symbol SymSquareClose }
<0, expr> \(                                                { symbol SymParenOpen }
<0, expr> \)                                                { symbol SymParenClose }

{
-- | Our monadic wrapper for `alexScan`.
scan :: Parser Token
scan = do
  input <- getInput
  code <- startCode
  case alexScan input code of
    AlexEOF -> pure EOF
    AlexError (AlexInput pos _ inp _) ->
      parseError $ InvalidLexeme pos inp
    AlexSkip rest _ -> do
      advance rest
      scan
    AlexToken rest nbytes action -> do
      advance rest
      action (slice nbytes input)

-- | The entry point to the lexer. Recursively calls `scan` to yield
-- tokens unti we hit EOF.
lexer :: Parser [Token]
lexer = do
  tok <- scan
  case tok of
    EOF -> pure []
    x -> (x :) <$> lexer
}
