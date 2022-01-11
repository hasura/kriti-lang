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

%name parser term
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
'null'      { TokIdentifier (Loc $$ "null" )}
'range'     { TokIdentifier (Loc $$ "range") }
'escapeUri' { TokIdentifier (Loc $$ "escapeUri") }
ident       { TokIdentifier $$ }

'\''        { TokSymbol (Loc $$ SymSingleQuote) }
':'         { TokSymbol (Loc $$ SymColon) }
'.'         { TokSymbol (Loc $$ SymDot) }
','         { TokSymbol (Loc $$ SymComma) }
'=='        { TokSymbol (Loc $$ SymEq) }
'>'         { TokSymbol (Loc $$ SymGt) }
'<'         { TokSymbol (Loc $$ SymLt) }
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

%left '<' '>' '==' '||' '&&' functions

%%

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
  : path_vector { uncurry Path $1 }
  | functions function_params { $1 $2 }
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
  : term { V.singleton $1 }
  | list_elements ',' term { V.snoc $1 $3 }

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
  : 's"' object_key '"e' ':' term { (unLoc $2, $5) }
  | 's"' '"e' ':' term { ("", $4) }

object_key :: { Loc T.Text }
object_key
  : object_key string { $1 <> $2 }
  | string { $1 }

operator :: { ValueExt }
operator
  : value '>' value  { Gt (locate $1 <> locate $3) $1 $3 }
  | value '<' value  { Lt (locate $1 <> locate $3) $1 $3 }
  | value '==' value { Eq (locate $1 <> locate $3) $1 $3 }
  | value '&&' value { And (locate $1 <> locate $3) $1 $3 }
  | value '||' value { Or (locate $1 <> locate $3) $1 $3 }

iff :: { ValueExt }
iff
  : '{{' 'if' value '}}' term '{{' 'else' '}}' term '{{' 'end' '}}' { Iff (locate $1 <> locate $12) $3 $5 $9 }

function_call :: { ValueExt }
function_call
  : '{{' functions function_params '}}' { $2 $3 }

functions :: { ValueExt -> ValueExt }
functions
  : 'escapeUri' { function EscapeURI (locate $1) }

function_params :: { ValueExt }
function_params
  : null { $1 }
  | boolean { $1 }
  | string_lit { $1 }
  | num_lit { $1 }
  | path_vector { uncurry Path $1 }
  | array { $1 }
  | object { $1 }
  | functions function_params { $1 $2 }
  | '(' function_params ')' { $2 }

range :: { ValueExt }
range
  : range_decl term '{{' 'end' '}}' { $1 (locate $5) $2 }

range_decl :: { Span -> ValueExt -> ValueExt }
range_decl
  : '{{' 'range' ident ',' ident ':=' path_vector '}}' { \s b -> Range (locate $1 <> s) (Just (unLoc $3)) (unLoc $5) (snd $7) b }
  | '{{' 'range' '_' ',' ident ':=' path_vector '}}' { \s b -> Range (locate $1 <> s) Nothing (unLoc $5) (snd $7) b }

path :: { ValueExt }
path
  : '{{' path_vector '}}' { Path (fst $2) (snd $2) }

path_vector :: { (Span, V.Vector Accessor) }
path_vector
  : ident path_tail { (locate $1 <> fst $2, V.cons (Obj (locate $1) (unLoc $1) Head) (snd $2))  }
  | ident { (locate $1, V.singleton (Obj (locate $1) (unLoc $1) Head)) }

path_tail :: { (Span, V.Vector Accessor) }
path_tail
  : path_element { (locate $1, V.singleton $1) } 
  | path_tail path_element { (fst $1 <> locate $2, V.snoc (snd $1) $2) }

path_element :: { Accessor }
path_element
  : '.' ident { Obj (locate $1 <> locate $2) (unLoc $2) DotAccess }
  | '[' '\'' string '\'' ']' { Obj (locate $1 <> locate $5) (unLoc $3) BracketAccess }
  | '[' int ']' { Arr (locate $1 <> locate $3) (unLoc $2) }

value :: { ValueExt }
value
  : num_lit { $1}
  | string_lit { $1 }
  | boolean  { $1 }
  | null { $1 }
  | path_vector { uncurry Path $1 }
  | iff { $1 }
  | operator { $1 }
  | '(' value ')' { $2 }

term :: { ValueExt }
term
  : num_lit       { $1 }
  | string_lit    { $1 }
  | boolean       { $1 }
  | null          { $1 }
  | array         { $1 }
  | object        { $1 }
  | path          { $1 }
  | iff           { $1 }
  | function_call { $1 }
  | range         { $1 }
  | '(' term ')'  { $2 }

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
}
