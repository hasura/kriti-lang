{
module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

}

%name parseKriti value
%tokentype { Token }
%error { parseError }

%token
ident       { Identifier $$}
string      { StringLit $$ }
number      { NumLit $$ }
'if'        { Identifier "if"}
'else'      { Identifier "else"}
'end'       { Identifier "end"}
'null'      { Identifier "null" }
'range'     { Identifier 'range' }
'escapeUri' { Identifier "escapeUri" }
'true'      { BoolLit True }
'false'     { BoolLit False }
'\''        { SingleQuote}
':'         { Colon }
'.'         { Dot }
','         { Comma }
'=='        { Eq }
'>'         { Gt }
'<'         { Lt }
'&&'        { And}
'||'        { Or }
'_'         { Underscore }
':='        { Assignment }
'{'         { CurlyOpen }
'}'         { CurlyClose }
'{{'        { DoubleCurlyOpen }
'}}'        { DoubleCurlyClose }
'['         { SquareOpen }
']'         { SquareClose }
'('         { ParenOpen }
')'         { ParenClose }

%left '<' '>' '==' '||' '&&' functions

%%

string_lit
  : string { StringLit $1 }

num_lit
  : number { NumLit $1 }

boolean
  : 'true'  { Boolean True }
  | 'false' { Boolean False }

null
  : 'null'  { Null }
  | '{' '}' { Null }

array
  : '[' list_elements ']' { Array $1 }
  | '[' ']'               { Array [] }

list_elements
  : value { [ $1 ] }
  | list_elements ',' value { $3 : $1 }

object
  : '{' object_fields '}' { Object (M.fromList $1) }

object_fields
  : object_field { [$1] }
  | object_fields ',' object_field { $3 : $1 }

object_field
  : string ':' value { ($1, $2) }

operator
  : predicate '>' predicate  { Gt $1 $3 }
  | predicate '<' predicate  { Lt $1 $3 }
  | predicate '==' predicate { Eq $1 $3 }
  | predicate '&&' predicate { And $1 $3 }
  | predicate '||' predicate { Or $1 $3 }

functions
  : 'escapeUri' value { EscapeURI $2 }

iff
  : '{{' 'if' predicate '}}' value '{{' 'else' '}}' value '{{' 'end' '}}' { Iff $3 $5 $9 }

predicate
  : path { $1 }
  | iff { $1 }
  | operator { $1 }
  | boolean  { $1 }
  | '(' predicate ')' { $2}

range
  : range_decl value '{{' 'end' '}}' { $1 $2 }

range_decl
  : '{{' 'range' ident ',' ident ':=' value '}}' { \b -> Range (Just $3) $5 $7 b }
  | '{{' 'range' '_' ',' ident ':=' value '}}' { \b -> Range Nothing $5 $7 b }

path
  : ident path_list { Path (Obj $1 : $2 )}

path_list
  : path_element { [ $1 ] }
  | path_list path_element { $2 : $1 }

path_element
  : '.' ident { Obj $2 }
  | '[' '\'' ident '\'' ']' { Obj $3 }
  | '[' number ']' { Arr $2 }

value : string_lit    { $1 }
      | num_lit       { $1 }
      | boolean       { $1 }
      | null          { $1 }
      | array         { $1 }
      | object        { $1 }
      | path          { $1 }
      | iff           { $1 }
      -- | operator      { $1 }
      | functions     { $1 }
      | range         { $1 }
      | '(' value ')' { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Accessor = Obj Text | Arr Int
  deriving (Show, Eq, Read)

data ValueExt
  = -- Core Aeson Terms
    Object (M.HashMap Text ValueExt)
  | Array (V.Vector ValueExt)
  | String Text
  | Number Scientific
  | Boolean Bool
  | Null
  | -- Extended Terms
    StringInterp [ValueExt]
  | Path [Accessor]
  | Iff ValueExt ValueExt ValueExt
  | Eq ValueExt ValueExt
  | Gt ValueExt ValueExt
  | Lt ValueExt ValueExt
  | And ValueExt ValueExt
  | Or ValueExt ValueExt
  | Member ValueExt ValueExt
  | Range (Maybe Text) Text [Accessor] ValueExt
  | EscapeURI ValueExt
  deriving (Show, Eq, Read)
}
