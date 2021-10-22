{
module Kriti.Parser where

import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Kriti.Error as E
import qualified Kriti.Lexer as L

}

%name parser value
%tokentype { L.TokenExt }
%error { parseError }
%monad { Either ParseError }
-- %lexer { L.lexer } { <eof> }

%token
'if'        { L.TokenExt (L.Identifier "if") _ _ _ }
'else'      { L.TokenExt (L.Identifier "else") _ _ _ }
'end'       { L.TokenExt (L.Identifier "end") _ _ _ }
'null'      { L.TokenExt (L.Identifier "null")  _ _ _ }
'range'     { L.TokenExt (L.Identifier "range")  _ _ _ }
'escapeUri' { L.TokenExt (L.Identifier "escapeUri")  _ _ _ }
'true'      { L.TokenExt (L.BoolLit True)  _ _ _ }
'false'     { L.TokenExt (L.BoolLit False)  _ _ _ }
ident       { L.TokenExt (L.Identifier $$) _ _ _ }
string      { L.TokenExt (L.StringTem $$)  _ _ _ }
number      { L.TokenExt (L.NumLit _ $$)  _ _ _ }
int         { L.TokenExt (L.IntLit _ $$)  _ _ _ }
'\''        { L.TokenExt L.SingleQuote  _ _ _ }
':'         { L.TokenExt L.Colon  _ _ _ }
'.'         { L.TokenExt L.Dot  _ _ _ }
','         { L.TokenExt L.Comma  _ _ _ }
'=='        { L.TokenExt L.Eq  _ _ _ }
'>'         { L.TokenExt L.Gt  _ _ _ }
'<'         { L.TokenExt L.Lt  _ _ _ }
'&&'        { L.TokenExt L.And _ _ _ }
'||'        { L.TokenExt L.Or  _ _ _ }
'_'         { L.TokenExt L.Underscore  _ _ _ }
':='        { L.TokenExt L.Assignment  _ _ _ }
'{'         { L.TokenExt L.CurlyOpen  _ _ _ }
'}'         { L.TokenExt L.CurlyClose  _ _ _ }
'{{'        { L.TokenExt L.DoubleCurlyOpen  _ _ _ }
'}}'        { L.TokenExt L.DoubleCurlyClose  _ _ _ }
'['         { L.TokenExt L.SquareOpen  _ _ _ }
']'         { L.TokenExt L.SquareClose  _ _ _ }
'('         { L.TokenExt L.ParenOpen  _ _ _ }
')'         { L.TokenExt L.ParenClose  _ _ _ }

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

iff
  : '{{' 'if' predicate '}}' value '{{' 'else' '}}' value '{{' 'end' '}}' { Iff $3 $5 $9 }

function_call
  : '{{' functions function_params '}}' { $2 $3 }

functions
  : 'escapeUri' { \p -> EscapeURI p }

function_params
  : null { $1 }
  | boolean { $1 }
  | string_lit { $1 }
  | num_lit { $1 }
  | path_vector { Path $1 }
  | array { $1 }
  | object { $1 }
  | functions function_params { $1 $2 }
  | '(' function_params ')' { $2 }

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
      | function_call { $1 }
      | range         { $1 }
      | '(' value ')' { $2 }

{
data ParseError = EmptyTokenStream | UnexpectedToken L.TokenExt
  deriving Show

instance E.RenderError ParseError where
  render EmptyTokenStream =
    E.RenderedError { _code = E.ParseErrorCode
                  , _message = "ParseError: Empty token stream."
                  , _span = (undefined, Nothing)
                  }

  render (UnexpectedToken L.TokenExt{..}) =
    let tok = L.serialize teType
    in E.RenderedError { _code = E.ParseErrorCode
                  , _message = "ParseError: Unexpected token '" <> tok <> "'."
                  , _span = (teStartPos, Just teEndPos)
                  }

parseError :: [L.TokenExt] -> Either ParseError a
parseError [] = Left EmptyTokenStream
parseError (tok:_) = Left $ UnexpectedToken tok

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
