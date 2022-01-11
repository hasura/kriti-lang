{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kriti.Parser.Token where

import qualified Data.List as L
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import qualified Kriti.Aeson.Compat as Compat
import Kriti.Parser.Spans

-- | The type of non literal/identifer symbols extracted from
-- source. This type is factored out of `Token` for clarity.
data Symbol
  = SymBling
  | SymColon
  | SymDot
  | SymComma
  | SymEq
  | SymNotEq
  | SymGt
  | SymGte
  | SymLt
  | SymLte
  | SymAnd
  | SymOr
  | SymSingleQuote
  | SymCurlyOpen
  | SymCurlyClose
  | SymDoubleCurlyOpen
  | SymDoubleCurlyClose
  | SymSquareOpen
  | SymSquareClose
  | SymParenOpen
  | SymParenClose
  | SymUnderscore
  | SymAssignment
  | SymStringBegin
  | SymStringEnd
  deriving (Show, Eq, Ord, Generic)

-- | The type of Lexeme Tokens. Lexemes can either be Symbols, identifiers or literals.
data Token
  = TokSymbol (Loc Symbol)
  | TokStringLit (Loc T.Text)
  | TokIdentifier (Loc T.Text)
  | TokNumLit T.Text (Loc Scientific)
  | TokIntLit T.Text (Loc Int)
  | TokBoolLit (Loc Bool)
  | EOF
  deriving (Show, Eq, Ord, Generic)

overLoc :: (forall a. Loc a -> Loc a) -> Token -> Token
overLoc f (TokSymbol loc) = TokSymbol $ f loc
overLoc f (TokStringLit loc) = TokStringLit $ f loc
overLoc f (TokIdentifier loc) = TokIdentifier $ f loc
overLoc f (TokNumLit txt loc) = TokNumLit txt $ f loc
overLoc f (TokIntLit txt loc) = TokIntLit txt $ f loc
overLoc f (TokBoolLit loc) = TokBoolLit $ f loc
overLoc _ EOF = EOF

serialize :: Token -> T.Text
serialize = \case
  TokStringLit str -> "\"" <> unLoc str <> "\""
  TokIdentifier iden -> unLoc iden
  TokIntLit str _ -> str
  TokNumLit str _ -> str
  TokBoolLit (Loc _ True) -> "true"
  TokBoolLit (Loc _ False) -> "false"
  TokSymbol (Loc _ SymBling) -> "$"
  TokSymbol (Loc _ SymColon) -> ":"
  TokSymbol (Loc _ SymDot) -> "."
  TokSymbol (Loc _ SymComma) -> ","
  TokSymbol (Loc _ SymSingleQuote) -> "'"
  TokSymbol (Loc _ SymDoubleCurlyOpen) -> "{{"
  TokSymbol (Loc _ SymDoubleCurlyClose) -> "}}"
  TokSymbol (Loc _ SymEq) -> "=="
  TokSymbol (Loc _ SymNotEq) -> "!="
  TokSymbol (Loc _ SymGt) -> ">"
  TokSymbol (Loc _ SymLt) -> "<"
  TokSymbol (Loc _ SymGte) -> ">="
  TokSymbol (Loc _ SymLte) -> "<="
  TokSymbol (Loc _ SymAnd) -> "&&"
  TokSymbol (Loc _ SymOr) -> "||"
  TokSymbol (Loc _ SymCurlyOpen) -> "{"
  TokSymbol (Loc _ SymCurlyClose) -> "}"
  TokSymbol (Loc _ SymSquareOpen) -> "["
  TokSymbol (Loc _ SymSquareClose) -> "]"
  TokSymbol (Loc _ SymParenOpen) -> "("
  TokSymbol (Loc _ SymParenClose) -> ")"
  TokSymbol (Loc _ SymUnderscore) -> "_"
  TokSymbol (Loc _ SymAssignment) -> ":="
  TokSymbol (Loc _ SymStringBegin) -> "\""
  TokSymbol (Loc _ SymStringEnd) -> "\""
  EOF -> ""

-- | Path lookups are represented as a stack of object and array lookups. eg., `Vector Accessor`.
data Accessor = Obj Span T.Text | Arr Span Int
  deriving (Show, Eq, Read)

renderAccessor :: Accessor -> T.Text
renderAccessor = \case
  -- TODO: Doesn't correctly account for `['foo bar']` object lookup syntax
  Obj _ txt -> txt
  Arr _ i -> T.pack $ show i

-- TODO: Should not insert a '.' when encountering an 'Arr' or square bracket object lookup
renderPath :: V.Vector Accessor -> T.Text
renderPath = mconcat . L.intersperse "." . V.toList . fmap renderAccessor

-- | The Kriti AST type. Kriti templates are parsed into `ValueExt`
-- terms which are then evaluated and converted into Aeson `Value`
-- terms.
data ValueExt
  = -- | Core Aeson Terms
    Object Span (Compat.Object ValueExt)
  | Array Span (V.Vector ValueExt)
  | String Span T.Text
  | Number Span Scientific
  | Boolean Span Bool
  | Null Span
  | -- | Extended Kriti Terms
    StringTem Span (V.Vector ValueExt)
  | Path Span (V.Vector Accessor)
  | Iff Span ValueExt ValueExt ValueExt
  | Eq Span ValueExt ValueExt
  | NotEq Span ValueExt ValueExt
  | Gt Span ValueExt ValueExt
  | Gte Span ValueExt ValueExt
  | Lt Span ValueExt ValueExt
  | Lte Span ValueExt ValueExt
  | And Span ValueExt ValueExt
  | Or Span ValueExt ValueExt
  | Not Span ValueExt
  | Member Span ValueExt ValueExt
  | Range Span (Maybe T.Text) T.Text (V.Vector Accessor) ValueExt
  | EscapeURI Span ValueExt
  deriving (Show, Eq, Read, Generic)

instance Located ValueExt where
  locate = \case
    Object s _ -> s
    Array s _ -> s
    String s _ -> s
    Number s _ -> s
    Boolean s _ -> s
    Null s -> s
    StringTem s _ -> s
    Path s _ -> s
    Iff s _ _ _ -> s
    Eq s _ _ -> s
    NotEq s _ _ -> s
    Gt s _ _ -> s
    Lt s _ _ -> s
    Gte s _ _ -> s
    Lte s _ _ -> s
    And s _ _ -> s
    Or s _ _ -> s
    Not s _ -> s
    Member s _ _ -> s
    Range s _ _ _ _ -> s
    EscapeURI s _ -> s

instance Located Accessor where
  locate = \case
    Obj s _ -> s
    Arr s _ -> s
