{
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module Kriti.Parser.Grammar where

import Control.Monad.State (gets)
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import qualified Kriti.Aeson.Compat as Compat
import qualified Kriti.Error as E
import qualified Kriti.Parser.Lexer as L
import Kriti.Parser.Monad
import Kriti.Parser.Token
import Kriti.Parser.Spans

}

%name parser expr
%tokentype { Token }
%monad { Parser }
%error { failure }

%token
number      { TokNumLit _ $$ }
int         { TokIntLit _ $$ }
'true'      { TokBoolLit $$ }
'false'     { TokBoolLit $$ }

's"'        { TokSymbol (Loc $$ SymStringBegin) }
'"e'        { TokSymbol (Loc $$ SymStringEnd) }
string      { TokStringLit $$ }

'if'        { TokIdentifier (Loc $$ "if") }
'else'      { TokIdentifier (Loc $$ "else") }
'end'       { TokIdentifier (Loc $$ "end") }
'null'      { TokIdentifier (Loc $$ "null" ) }
'range'     { TokIdentifier (Loc $$ "range") }
'in'        { TokIdentifier (Loc $$ "in") }
ident       { TokIdentifier $$ }

'\''        { TokSymbol (Loc $$ SymSingleQuote) }
':'         { TokSymbol (Loc $$ SymColon) }
'.'         { TokSymbol (Loc $$ SymDot) }
','         { TokSymbol (Loc $$ SymComma) }
'?'         { TokSymbol (Loc $$ SymQuestionMark) }
'??'        { TokSymbol (Loc $$ SymDoubleQuestionMark) }
'=='        { TokSymbol (Loc $$ SymEq) }
'!='        { TokSymbol (Loc $$ SymNotEq) }
'>'         { TokSymbol (Loc $$ SymGt) }
'<'         { TokSymbol (Loc $$ SymLt) }
'<='        { TokSymbol (Loc $$ SymLte) }
'>='        { TokSymbol (Loc $$ SymGte) }
'&&'        { TokSymbol (Loc $$ SymAnd) }
'||'        { TokSymbol (Loc $$ SymOr) }
'_'         { TokSymbol (Loc $$ SymUnderscore) }
':='        { TokSymbol (Loc $$ SymAssignment) }
'{'         { TokSymbol (Loc $$ SymCurlyOpen) }
'}'         { TokSymbol (Loc $$ SymCurlyClose) }
'{{'        { TokSymbol (Loc $$ SymDoubleCurlyOpen) }
'}}'        { TokSymbol (Loc $$ SymDoubleCurlyClose) }
'['         { TokSymbol (Loc $$ SymSquareOpen) }
']'         { TokSymbol (Loc $$ SymSquareClose) }
'('         { TokSymbol (Loc $$ SymParenOpen) }
')'         { TokSymbol (Loc $$ SymParenClose) }

%right 'in'
%right '||'
%right '&&'
%right '??' 
%right 'if'

%right LOOSE
%right TIGHT
%right '['

%nonassoc '==' '>=' '<=' '!=' '>' '<' -- 4
%nonassoc int number 'true' 'false' ident '(' ')' '{{' '}}'

%%

------------------------------------------------------------------------
-- TOP

expr :: { ValueExt }
  : expr '>'  expr { Gt (locate $1 <> locate $3) $1 $3 }
  | expr '<'  expr { Lt (locate $1 <> locate $3) $1 $3 }
  | expr '>=' expr { Gte (locate $1 <> locate $3) $1 $3 }
  | expr '<=' expr { Lte (locate $1 <> locate $3) $1 $3 }
  | expr '!=' expr { NotEq (locate $1 <> locate $3) $1 $3 }
  | expr '==' expr { Eq (locate $1 <> locate $3) $1 $3 }
  | expr '&&' expr { And (locate $1 <> locate $3) $1 $3 }
  | expr '||' expr { Or (locate $1 <> locate $3) $1 $3 }
  | expr 'in' expr { In (locate $1 <> locate $3) $1 $3 }
  | expr '??' expr { Defaulting (locate $1 <> locate $3) $1 $3 }
  | atom { $1 }
  | var '(' expr ')' %prec TIGHT { Ap (locate $1 <> locate $4) $1 $3 }
  | '(' expr ')' { $2 }
  | '{{' expr '}}' { $2 }

atom :: { ValueExt }
  : var { $1 }
  | string_template { $1 }
  | num_lit { $1 }
  | boolean { $1 }
  | null { $1 }
  | array { $1 }
  | object { $1 }
  | field { $1 }
  | iff { $1 }
  | range { $1 }
  | '{{' var '}}' { $2 }
  | '{{' field '}}' { $2 }

------------------------------------------------------------------------

var :: { ValueExt }
  : ident { Var (locate $1) (unLoc $1) }

------------------------------------------------------------------------

string_template :: { ValueExt }
  : 's"' string_template_ '"e' { StringTem (locate $1 <> $3) $2 }
  | 's"' '"e' { StringTem (locate $1 <> locate $2) mempty }

------------------------------------------------------------------------

string_template_ :: { V.Vector ValueExt }
  -- Template to the right
  : string_template_ '{{' template '}}' { V.snoc $1 $3 }
  -- String Lit to the right
  | string_template_ string { V.snoc $1 (String (locate $2) (unLoc $2)) }
  -- Template Base Case
  | '{{' template '}}' { V.singleton $2 }
  -- String Base Case
  | string { V.singleton (String (locate $1) (unLoc $1))}

template :: { ValueExt }
  : boolean { $1 }
  | num_lit { $1 }
  | var     { $1 }
  | field { $1 }
  | var '(' expr ')' %prec TIGHT { Ap (locate $1 <> locate $4) $1 $3 }

------------------------------------------------------------------------

num_lit :: { ValueExt }
  : number { Number (locate $1) (unLoc $1)  }
  | int { Number (locate $1) (S.scientific (fromIntegral (unLoc $1)) 0) }

------------------------------------------------------------------------

boolean :: { ValueExt }
  : 'true'  { Boolean (locate $1) (unLoc $1) }
  | 'false' { Boolean (locate $1) (unLoc $1) }

------------------------------------------------------------------------

null :: { ValueExt }
  : 'null'  { Null (locate $1) }

------------------------------------------------------------------------

array :: { ValueExt }
  : '[' list_elements ']' { Array (locate $1 <> locate $3) $2 }
  | '[' ']'               { Array (locate $1 <> locate $2) V.empty }

list_elements :: { V.Vector ValueExt }
  : expr { V.singleton $1 }
  | list_elements ',' expr { V.snoc $1 $3 }

------------------------------------------------------------------------

object :: { ValueExt }
  : '{' object_fields '}' { Object (locate $1 <> locate $3) (Compat.fromList $2) }
  | '{' '}'               { Object (locate $1 <> locate $2) mempty }

object_fields :: { [(T.Text, ValueExt)] }
  : object_field { [$1] }
  | object_fields ',' object_field { $3 : $1 }

object_field :: { (T.Text, ValueExt) }
  : 's"' object_key '"e' ':' expr { (unLoc $2, $5) }
  | 's"' '"e' ':' expr { ("", $4) }

-- | NOTE: String Literals can be fragmented coming out of the lexer.
-- 'object_key' has a recursive production rule to smoosh those
-- fragments together.
object_key :: { Loc T.Text }
  : object_key string { $1 <> $2 }
  | string { $1 }

------------------------------------------------------------------------

field :: { ValueExt }
  : atom '.' ident { Field (locate $1 <> locate $3) NotOptional $1 (String (locate $3) (unLoc $3)) }
  | atom '?' '.' ident { Field (locate $1 <> locate $4) Optional $1 (String (locate $4) (unLoc $4)) }
  | atom '[' '\'' string_template_ '\''  ']' { Field (locate $1 <> locate $6) NotOptional $1 (StringTem (locate $3 <> locate $5) $4) }
  | atom '[' expr ']' { Field (locate $1 <> locate $4) NotOptional $1  $3 }
  | atom '?' '[' '\'' ident '\''  ']' { Field (locate $1 <> locate $7) Optional $1 (String (locate $5) (unLoc $5)) }
  | atom '?' '[' '\'' string_template_ '\''  ']' { Field (locate $1 <> locate $7) Optional $1 (StringTem (locate $4 <> locate $6) $5) }
  | atom '?' '[' expr ']' { Field (locate $1 <> locate $5) Optional $1 $4 }

------------------------------------------------------------------------

iff :: { ValueExt }
  : '{{' 'if' expr '}}' expr '{{' 'else' '}}' expr '{{' 'end' '}}' { Iff (locate $1 <> locate $12) $3 $5 $9 }

------------------------------------------------------------------------

-- TODO: switch 'app' to 'expr'
range :: { ValueExt }
range
  : '{{' 'range' mident ',' ident ':=' expr '}}' expr '{{' 'end' '}}' { Range (locate $1 <> locate $12) (fmap unLoc $3) (unLoc $5) $7 $9 }

mident :: { Maybe (Loc T.Text) }
mident
  : '_' { Nothing }
  | ident { Just $1 }

------------------------------------------------------------------------

{
failure :: [Token] -> Parser a
failure [] = do
  sp <- location
  src <- gets parseSource
  parseError $ EmptyTokenStream sp src
failure (tok:_) = do
  sp <- location
  src <- gets parseSource
  -- TODO: fix source position capture here. I think we need the prior span.
  parseError $ UnexpectedToken (Loc sp tok) src

buildFunc :: (Span -> ValueExt -> ValueExt) -> Span -> ValueExt -> ValueExt
buildFunc f sp param = f (sp <> locate param) param
}
