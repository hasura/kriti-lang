{
module Kriti.Parser.Grammar where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
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

's"'        { StringBegin $$ }
'"e'        { StringEnd $$ }
string      { TokStringLit $$ }
's{'        { TemplateBegin $$ }
'}e'        { TemplateEnd $$ }

'if'        { TokIdentifier (Loc $$ "if") }
'else'      { TokIdentifier (Loc $$ "else") }
'end'       { TokIdentifier (Loc $$ "end") }
'null'      { TokIdentifier (Loc $$ "null" )}
'range'     { TokIdentifier (Loc $$ "range") }
'escapeUri' { TokIdentifier (Loc $$ "escapeUri") }
ident       { TokIdentifier $$ }

'\''        { TokSymbol SymSingleQuote $$ }
':'         { TokSymbol SymColon $$ }
'.'         { TokSymbol SymDot $$ }
','         { TokSymbol SymComma $$ }
'=='        { TokSymbol SymEq $$ }
'>'         { TokSymbol SymGt $$ }
'<'         { TokSymbol SymLt $$ }
'&&'        { TokSymbol SymAnd $$ }
'||'        { TokSymbol SymOr $$ }
'_'         { TokSymbol SymUnderscore $$ }
':='        { TokSymbol SymAssignment $$ }
'{'         { TokSymbol SymCurlyOpen $$ }
'}'         { TokSymbol SymCurlyClose $$ }
'{{'        { TokSymbol SymDoubleCurlyOpen $$ }
'}}'        { TokSymbol SymDoubleCurlyClose $$ }
'['         { TokSymbol SymSquareOpen $$ }
']'         { TokSymbol SymSquareClose $$ }
'('         { TokSymbol SymParenOpen $$ }
')'         { TokSymbol SymParenClose $$ }

%left '<' '>' '==' '||' '&&' functions

%%

string_lit :: { ValueExt }
string_lit
  : 's"' string_template '"e' { StringTem (locate $1 <> $3) $2 }

string_template :: { V.Vector ValueExt }
string_template
  -- Template to the right
  : string_template 's{' template '}e' { V.snoc $1 $3 }
  -- String Lit to the right
  | string_template string { V.snoc $1 (String (locate $2) (unlocate $2)) }
  -- Template Base Case
  | 's{' template '}e' { V.singleton $2 }
  -- String Base Case
  | string { V.singleton (String (locate $1) (unlocate $1))}

template :: { ValueExt }
template
  : path_vector { uncurry Path $1 }

num_lit :: { ValueExt }
num_lit
  : number { Number (locate $1) (unlocate $1)  }
  | int { Number (locate $1) (S.scientific (fromIntegral (unlocate $1)) 0) }

boolean :: { ValueExt }
boolean
  : 'true'  { Boolean (locate $1) (unlocate $1) }
  | 'false' { Boolean (locate $1) (unlocate $1) }

null :: { ValueExt }
null
  : 'null'  { Null (locate $1) }
  | '{' '}' { Null (locate $1 <> locate $2) }

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
: '{' object_fields '}' { Object (locate $1 <> locate $3) (M.fromList $2) }

object_fields :: { [(T.Text, ValueExt)] }
object_fields
  : object_field { [$1] }
  | object_fields ',' object_field { $3 : $1 }

object_field :: { (T.Text, ValueExt) }
object_field
  -- TODO: Key should be a String Literal
  : ident ':' term { (unlocate $1, $3) }

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
  : 'escapeUri' { EscapeURI (locate $1) }

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
  : '{{' 'range' ident ',' ident ':=' path_vector '}}' { \s b -> Range (locate $1 <> s) (Just (unlocate $3)) (unlocate $5) (snd $7) b }
  | '{{' 'range' '_' ',' ident ':=' path_vector '}}' { \s b -> Range (locate $1 <> s) Nothing (unlocate $5) (snd $7) b }

path :: { ValueExt }
path
  : '{{' path_vector '}}' { Path (locate $1 <> locate $3) (snd $2) }

path_vector :: { (Span, V.Vector Accessor) }
path_vector
  : ident path_tail { (locate $1 <> fst $2, V.cons (Obj (locate $1) (unlocate $1)) (snd $2))  }
  | ident { (locate $1, V.singleton (Obj (locate $1) (unlocate $1))) }

path_tail :: { (Span, V.Vector Accessor) }
path_tail
  : path_element { (locate $1, V.singleton $1) } 
  | path_tail path_element { (fst $1 <> locate $2, V.snoc (snd $1) $2) }

path_element :: { Accessor }
path_element
  : '.' ident { Obj (locate $1 <> locate $2) (unlocate $2) }
  | '[' '\'' ident '\'' ']' { Obj (locate $1 <> locate $5) (unlocate $3) }
  | '[' int ']' { Arr (locate $1 <> locate $3) (unlocate $2) }

value :: { ValueExt }
value
  : path_vector { uncurry Path $1 }
  | iff { $1 }
  | operator { $1 }
  | boolean  { $1 }
  | num_lit { $1}
  | string_lit { $1 }
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
  | '(' term ')' { $2 }

{
failure :: [Token] -> Parser a
failure [] = parseError EmptyTokenStream
failure (tok:_) = do
  sp <- location
  parseError $ UnexpectedToken (Loc sp tok)
}
