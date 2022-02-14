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
'escapeUri' { TokIdentifier (Loc $$ "escapeUri") }
'not'       { TokIdentifier (Loc $$ "not") }
'in'        { TokIdentifier (Loc $$ "in") }
'customFunc'{ TokIdentifier (Loc $$ "customFunc") }
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
%nonassoc '>' '<' '<=' '>=' '==' '!=' '&&' '||' 
%left '??' 
%left 'not' 'escapeUri' 'customFunc'

%%

expr :: { ValueExt }
expr
  : json { $1 } 
  | kriti { $1 }

------------------------------------------------------------------------
-- KRITI

kriti :: { ValueExt }
kriti
  : '{{' path '}}' { $2 }
  | '{{' operator '}}' { $2 }
  | '{{' function '}}' { $2 }
  | '(' kriti ')' { $2 }
  | range { $1 }
  | iff { $1 }

kritiValue :: { ValueExt }
  : path { $1 }
  | operator { $1 } 
  | function { $1 }
  | range { $1 }
  | iff { $1 }
  | json { $1 }
  | '(' kritiValue ')' { $2 }

path :: { ValueExt }
path
  : path_vector { Path (fst $1) (snd $1) }

path_vector :: { (Span, V.Vector Accessor) }
path_vector
  : ident path_tail { (locate $1 <> fst $2, V.cons (Obj (locate $1) NotOptional (unLoc $1) Head) (snd $2))  }
  | ident { (locate $1, V.singleton (Obj (locate $1) NotOptional (unLoc $1) Head)) }

path_tail :: { (Span, V.Vector Accessor) }
path_tail
  : path_element { (locate $1, V.singleton $1) } 
  | path_tail path_element { (fst $1 <> locate $2, V.snoc (snd $1) $2) }

path_element :: { Accessor }
path_element
  : '.' ident { Obj (locate $1 <> locate $2) NotOptional (unLoc $2) DotAccess }
  | '?' '.' ident { Obj (locate $1 <> locate $3) Optional (unLoc $3) DotAccess }
  | '[' '\'' string '\'' ']' { Obj (locate $1 <> locate $5) NotOptional (unLoc $3) BracketAccess }
  | '?' '[' '\'' string '\'' ']' { Obj (locate $1 <> locate $6) Optional (unLoc $4) BracketAccess }
  | '[' int ']' { Arr (locate $1 <> locate $3) NotOptional (unLoc $2) }
  | '?' '[' int ']' { Arr (locate $1 <> locate $4) Optional (unLoc $3) }

operator :: { ValueExt }
operator
  : kritiValue '>'  kritiValue { Gt (locate $1 <> locate $3) $1 $3 }
  | kritiValue '<'  kritiValue { Lt (locate $1 <> locate $3) $1 $3 }
  | kritiValue '>=' kritiValue { Gte (locate $1 <> locate $3) $1 $3 }
  | kritiValue '<=' kritiValue { Lte (locate $1 <> locate $3) $1 $3 }
  | kritiValue '!=' kritiValue { NotEq (locate $1 <> locate $3) $1 $3 }
  | kritiValue '==' kritiValue { Eq (locate $1 <> locate $3) $1 $3 }
  | kritiValue '&&' kritiValue { And (locate $1 <> locate $3) $1 $3 }
  | kritiValue '||' kritiValue { Or (locate $1 <> locate $3) $1 $3 }
  | kritiValue 'in' kritiValue { In (locate $1 <> locate $3) $1 $3 }
  | kritiValue '??' kritiValue { Defaulting (locate $1 <> locate $3) $1 $3 }

function :: { ValueExt }
function
  : 'escapeUri' kritiValue { buildFunc EscapeURI (locate $1) $2 }
  | 'not' kritiValue { buildFunc Not (locate $1) $2 }
  | 'customFunc' kritiValue { buildFunc CustomFunc (locate $1) $2 }

range :: { ValueExt }
range
  : '{{' 'range' mident ',' ident ':=' path_vector '}}' expr '{{' 'end' '}}' { Range (locate $1 <> locate $12) (fmap unLoc $3) (unLoc $5) (snd $7) $9 }

mident :: { Maybe (Loc T.Text) }
mident
  : '_' { Nothing }
  | ident { Just $1 }

iff :: { ValueExt }
iff
  : '{{' 'if' kritiValue '}}' expr '{{' 'else' '}}' expr '{{' 'end' '}}' { Iff (locate $1 <> locate $12) $3 $5 $9 }

------------------------------------------------------------------------
-- JSON

json :: { ValueExt }
json
  : string_lit { $1 }
  | num_lit { $1 }
  | boolean { $1 }
  | null { $1 }
  | array { $1 }
  | object { $1 }

string_lit :: { ValueExt }
string_lit
  : 's"' string_template '"e' { StringTem (locate $1 <> $3) $2 }
  | 's"' '"e' { StringTem (locate $1 <> locate $2) mempty }
  
string_template :: { V.Vector ValueExt }
string_template
  -- Template to the right
  : string_template '{{' template '}}' { V.snoc $1 $3 }
  -- String Lit to the right
  | string_template string { V.snoc $1 (String (locate $2) (unLoc $2)) }
  -- Template Base Case
  | '{{' template '}}' { V.singleton $2 }
  -- String Base Case
  | string { V.singleton (String (locate $1) (unLoc $1))}

template :: { ValueExt }
template
  : path { $1 }
  | function { $1 }
  | boolean { $1 }
  | num_lit { $1 }

num_lit :: { ValueExt }
num_lit
  : number { Number (locate $1) (unLoc $1)  }
  | int { Number (locate $1) (S.scientific (fromIntegral (unLoc $1)) 0) }

boolean :: { ValueExt }
boolean
  : 'true'  { Boolean (locate $1) (unLoc $1) }
  | 'false' { Boolean (locate $1) (unLoc $1) }

null :: { ValueExt }
null
  : 'null'  { Null (locate $1) }

array :: { ValueExt }
array
  : '[' list_elements ']' { Array (locate $1 <> locate $3) $2 }
  | '[' ']'               { Array (locate $1 <> locate $2) V.empty }

list_elements :: { V.Vector ValueExt }
list_elements
  : expr { V.singleton $1 }
  | list_elements ',' expr { V.snoc $1 $3 }

object :: { ValueExt }
object
  : '{' object_fields '}' { Object (locate $1 <> locate $3) (Compat.fromList $2) }
  | '{' '}'               { Object (locate $1 <> locate $2) mempty }

object_fields :: { [(T.Text, ValueExt)] }
object_fields
  : object_field { [$1] }
  | object_fields ',' object_field { $3 : $1 }

object_field :: { (T.Text, ValueExt) }
object_field
  : 's"' object_key '"e' ':' expr { (unLoc $2, $5) }
  | 's"' '"e' ':' expr { ("", $4) }

object_key :: { Loc T.Text }
object_key
  : object_key string { $1 <> $2 }
  | string { $1 }

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
