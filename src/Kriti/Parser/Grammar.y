{
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module Kriti.Parser.Grammar where

import Control.Monad.State (gets)
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import Data.Functor.Sum
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
ident       { TokIdentifier $$ }

'\''        { TokSymbol (Loc $$ SymSingleQuote) }
':'         { TokSymbol (Loc $$ SymColon) }
'.'         { TokSymbol (Loc $$ SymDot) }
','         { TokSymbol (Loc $$ SymComma) }
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

%right in
%nonassoc '<' '>' '<=' '>=' '!=' '==' '||' '&&'

%%

string_lit :: { Expr }
string_lit
  : 's"' string_template '"e' { mkKriti (StringTemF (locate $1 <> $3) $2) }
  | 's"' '"e' { mkKriti (StringTemF (locate $1 <> locate $2) mempty) }

string_template :: { V.Vector Expr }
string_template
  -- Template to the right
  : string_template '{{' template '}}' { V.snoc $1 $3 }
  -- String Lit to the right
  | string_template string { V.snoc $1 (mkJson (String (locate $2) (unLoc $2))) }
  -- Template Base Case
  | '{{' template '}}' { V.singleton $2 }
  -- String Base Case
  | string { V.singleton (mkJson (String (locate $1) (unLoc $1)))}

template :: { Expr }
template
--  : path_vector { uncurry PathF $1 }
--  | functions function_params { $1 $2 }
  : boolean { $1 }
  | num_lit { $1 }

num_lit :: { Expr }
num_lit
  : number { mkJson (Number (locate $1) (unLoc $1))  }
  | int { mkJson (Number (locate $1) (S.scientific (fromIntegral (unLoc $1)) 0)) }

boolean :: { Expr }
boolean
  : 'true'  { mkJson (Boolean (locate $1) (unLoc $1)) }
  | 'false' { mkJson (Boolean (locate $1) (unLoc $1)) }

null :: { Expr }
null
  : 'null'  { mkJson (Null (locate $1)) }

array :: { Expr }
array
  : '[' list_elements ']' { mkJson (Array (locate $1 <> locate $3) $2) }
  | '[' ']'               { mkJson (Array (locate $1 <> locate $2) V.empty) }

list_elements :: { V.Vector Expr }
list_elements
  : term { V.singleton $1 }
  | list_elements ',' term { V.snoc $1 $3 }

object :: { Expr }
object
  : '{' object_fields '}' { mkJson (Object (locate $1 <> locate $3) (M.fromList $2)) }
  | '{' '}'               { mkJson (Object (locate $1 <> locate $2) mempty) }

object_fields :: { [(T.Text, Expr)] }
object_fields
  : object_field { [$1] }
  | object_fields ',' object_field { $3 : $1 }

object_field :: { (T.Text, Expr) }
object_field
  : 's"' object_key '"e' ':' term { (unLoc $2, $5) }
  | 's"' '"e' ':' term { ("", $4) }

object_key :: { Loc T.Text }
object_key
  : object_key string { $1 <> $2 }
  | string { $1 }

operator :: { Expr }
operator
  : value '>' value  { mkKriti (GtF (locate $1 <> locate $3) $1 $3)}
  | value '<' value  { mkKriti (LtF (locate $1 <> locate $3) $1 $3) }
  | value '>=' value { mkKriti (GteF (locate $1 <> locate $3) $1 $3) }
  | value '<=' value { mkKriti (LteF (locate $1 <> locate $3) $1 $3) }
  | value '!=' value { mkKriti (NotEqF (locate $1 <> locate $3) $1 $3) }
  | value '==' value { mkKriti (EqF (locate $1 <> locate $3) $1 $3) }
  | value '&&' value { mkKriti (AndF (locate $1 <> locate $3) $1 $3) }
  | value '||' value { mkKriti (OrF (locate $1 <> locate $3) $1 $3) }
  | value 'in' value { mkKriti (InF (locate $1 <> locate $3) $1 $3) }

iff :: { Expr }
iff
  : '{{' 'if' value '}}' term '{{' 'else' '}}' term '{{' 'end' '}}' { mkKriti (IffF (locate $1 <> locate $12) $3 $5 $9) }

function_call :: { Expr }
function_call
  : '{{' functions function_params '}}' { $2 $3 }

functions :: { Expr -> Expr }
functions
  : 'escapeUri' { buildFunc (fmap (fmap mkKriti) EscapeURIF) (locate $1) }
  | 'not' { buildFunc (fmap (fmap mkKriti) NotF) (locate $1) }

function_params :: { Expr }
function_params
  : null { $1 }
  | boolean { $1 }
  | string_lit { $1 }
  | num_lit { $1 }
  | path_vector { mkKriti (uncurry PathF $1) }
  | array { $1 }
  | object { $1 }
  | functions function_params { $1 $2 }
  | '(' function_params ')' { $2 }

range :: { Expr }
range
  : range_decl term '{{' 'end' '}}' { $1 (locate $5) $2 }

range_decl :: { Span -> Expr -> Expr }
range_decl
  : '{{' 'range' ident ',' ident ':=' path_vector '}}' { \s b -> mkKriti (RangeF (locate $1 <> s) (Just (unLoc $3)) (unLoc $5) (snd $7) b) }
  | '{{' 'range' '_' ',' ident ':=' path_vector '}}' { \s b -> mkKriti (RangeF (locate $1 <> s) Nothing (unLoc $5) (snd $7) b) }

path :: { Expr }
path
  : '{{' path_vector '}}' { mkKriti (PathF (fst $2) (snd $2)) }

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

value :: { Expr }
value
  : num_lit	  { $1 }
  | string_lit	  { $1 }
  | boolean	  { $1 }
  | null	  { $1 }
  | array	  { $1 }
  | object	  { $1 }
  | path_vector	  { mkKriti (uncurry PathF $1) }
  | iff		  { $1 }
  | operator	  { $1 }
  | range         { $1 }
  | functions function_params { $1 $2 }
  | '(' value ')' { $2 }

term :: { Expr }
term
  : num_lit       { $1 }
  | string_lit    { $1 }
  | boolean       { $1 }
  | null          { $1 }
  | array         { $1 }
  | object        { $1 }
  | path          { $1 }
  | iff           { $1 }
  | operator      { $1 }
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

buildFunc :: (Span -> Expr -> Expr) -> Span -> Expr -> Expr
buildFunc f sp param = f (sp <> locate param) param
}
