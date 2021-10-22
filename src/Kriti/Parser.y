{
module Kriti.Parser where

import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Kriti.Lexer as L

}

%name parser value
%tokentype { L.Token }
%error { parseError }

%token
'if'        { L.Identifier "if"}
'else'      { L.Identifier "else"}
'end'       { L.Identifier "end"}
'null'      { L.Identifier "null" }
'range'     { L.Identifier "range" }
'escapeUri' { L.Identifier "escapeUri" }
'true'      { L.BoolLit True }
'false'     { L.BoolLit False }
ident       { L.Identifier $$}
string      { L.StringTem $$ }
number      { L.NumLit _ $$ }
int         { L.IntLit _ $$ }
'\''        { L.SingleQuote}
':'         { L.Colon }
'.'         { L.Dot }
','         { L.Comma }
'=='        { L.Eq }
'>'         { L.Gt }
'<'         { L.Lt }
'&&'        { L.And}
'||'        { L.Or }
'_'         { L.Underscore }
':='        { L.Assignment }
'{'         { L.CurlyOpen }
'}'         { L.CurlyClose }
'{{'        { L.DoubleCurlyOpen }
'}}'        { L.DoubleCurlyClose }
'['         { L.SquareOpen }
']'         { L.SquareClose }
'('         { L.ParenOpen }
')'         { L.ParenClose }

%left '<' '>' '==' '||' '&&' functions

%%

string_lit
  : string { String $1 }

num_lit
  : number { Number $1 }
| int { Number (S.scientific (fromIntegral $1) 0)}

boolean
  : 'true'  { Boolean True }
  | 'false' { Boolean False }

null
  : 'null'  { Null }
  | '{' '}' { Null }

array
  : '[' list_elements ']' { Array $2 }
  | '[' ']'               { Array V.empty }

list_elements
  : value { V.singleton $1 }
  | list_elements ',' value { V.snoc $1 $3 }

object
  : '{' object_fields '}' { Object (M.fromList $2) }

object_fields
  : object_field { [$1] }
  | object_fields ',' object_field { $3 : $1 }

object_field
  : string ':' value { ($1, $3) }

operator
  : predicate '>' predicate  { Gt $1 $3 }
  | predicate '<' predicate  { Lt $1 $3 }
  | predicate '==' predicate { Eq $1 $3 }
  | predicate '&&' predicate { And $1 $3 }
  | predicate '||' predicate { Or $1 $3 }

functions
: '{{' 'escapeUri' value '}}' { EscapeURI $3 }

iff
  : '{{' 'if' predicate '}}' value '{{' 'else' '}}' value '{{' 'end' '}}' { Iff $3 $5 $9 }

predicate
  : path_vector { Path $1 }
  | iff { $1 }
  | operator { $1 }
  | boolean  { $1 }
  | num_lit { $1}
  | string_lit { $1 }
  | '(' predicate ')' { $2 }

range
  : range_decl value '{{' 'end' '}}' { $1 $2 }

range_decl
  : '{{' 'range' ident ',' ident ':=' path_vector '}}' { \b -> Range (Just $3) $5 $7 b }
  | '{{' 'range' '_' ',' ident ':=' path_vector '}}' { \b -> Range Nothing $5 $7 b }

path
 : '{{' path_vector '}}' { Path $2}

path_vector
 : ident path_tail { V.cons (Obj $1) $2 }
 | ident { V.singleton (Obj $1) }

path_tail
  : path_element { V.singleton $1 }
  | path_tail path_element { V.snoc $1 $2 }

path_element
  : '.' ident { Obj $2 }
  | '[' '\'' ident '\'' ']' { Obj $3 }
  | '[' int ']' { Arr $2 }

value : string_lit    { $1 }
      | num_lit       { $1 }
      | boolean       { $1 }
      | null          { $1 }
      | array         { $1 }
      | object        { $1 }
      | path          { $1 }
      | iff           { $1 }
      | functions     { $1 }
      | range         { $1 }
      | '(' value ')' { $2 }

{
parseError :: [L.Token] -> a
parseError toks = error $ "Parse error: " <> show toks

data Accessor = Obj T.Text | Arr Int
  deriving (Show, Eq, Read)

renderAccessor :: Accessor -> T.Text
renderAccessor = \case
  Obj txt -> txt
  Arr i -> T.pack $ show i

renderPath :: V.Vector Accessor -> T.Text
renderPath = mconcat . List.intersperse "." . V.toList . fmap renderAccessor

data ValueExt
  = -- Core Aeson Terms
    Object (M.HashMap T.Text ValueExt)
  | Array (V.Vector ValueExt)
  | String T.Text
  | Number S.Scientific
  | Boolean Bool
  | Null
  | -- | Extended Terms
    StringTem [ValueExt]
  | Path (V.Vector Accessor)
  | Iff ValueExt ValueExt ValueExt
  | Eq ValueExt ValueExt
  | Gt ValueExt ValueExt
  | Lt ValueExt ValueExt
  | And ValueExt ValueExt
  | Or ValueExt ValueExt
  | Member ValueExt ValueExt
  | Range (Maybe T.Text) T.Text (V.Vector Accessor) ValueExt
  | EscapeURI ValueExt
  deriving (Show, Eq, Read)
}
