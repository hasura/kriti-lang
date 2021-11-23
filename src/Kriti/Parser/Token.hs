{-# OPTIONS_GHC -fno-warn-orphans #-}
module Kriti.Parser.Token where

import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import Kriti.Parser.Spans

data Symbol =
    SymBling
  | SymColon
  | SymDot
  | SymComma
  | SymEq
  | SymGt
  | SymLt
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

data Token =
    TokSymbol Symbol Span
  | TokStringLit (Loc T.Text)
  | TokIdentifier (Loc T.Text)
  | TokNumLit T.Text (Loc Scientific)
  | TokIntLit T.Text (Loc Int)
  | TokBoolLit (Loc Bool)
  | EOF
  deriving (Show, Eq, Ord, Generic)

serialize :: Token -> T.Text
serialize = \case
  TokStringLit str ->  "\"" <> unlocate str <> "\""
  TokIdentifier iden -> unlocate iden
  TokIntLit str _ -> str
  TokNumLit str _ -> str
  TokBoolLit (Loc _ True) -> "true"
  TokBoolLit (Loc _ False) -> "false"
  TokSymbol SymBling _ -> "$"
  TokSymbol SymColon _ -> ":"
  TokSymbol SymDot _ -> "."
  TokSymbol SymComma _ -> ","
  TokSymbol SymSingleQuote _ -> "'"
  TokSymbol SymDoubleCurlyOpen _ -> "{{"
  TokSymbol SymDoubleCurlyClose _ -> "}}"
  TokSymbol SymEq _ -> "=="
  TokSymbol SymGt _ -> ">"
  TokSymbol SymLt _ -> "<"
  TokSymbol SymAnd _ -> "&&"
  TokSymbol SymOr _ -> "||"
  TokSymbol SymCurlyOpen _ -> "{"
  TokSymbol SymCurlyClose _ -> "}"
  TokSymbol SymSquareOpen _ -> "["
  TokSymbol SymSquareClose _ -> "]"
  TokSymbol SymParenOpen _ -> "("
  TokSymbol SymParenClose _ -> ")"
  TokSymbol SymUnderscore _ -> "_"
  TokSymbol SymAssignment _ -> ":="
  TokSymbol SymStringBegin _ -> "\""
  TokSymbol SymStringEnd _ -> "\""
  EOF -> ""

data Accessor = Obj Span T.Text | Arr Span Int
  deriving (Show, Eq, Read)

renderAccessor :: Accessor -> T.Text
renderAccessor = \case
  Obj _ txt -> txt
  Arr _ i -> T.pack $ show i

renderPath :: V.Vector Accessor -> T.Text
renderPath = mconcat . L.intersperse "." . V.toList . fmap renderAccessor

data ValueExt
  = -- | Core Aeson Terms
    Object Span (M.HashMap T.Text ValueExt)
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
  | Gt Span ValueExt ValueExt
  | Lt Span ValueExt ValueExt
  | And Span ValueExt ValueExt
  | Or Span ValueExt ValueExt
  | Member Span ValueExt ValueExt
  | Range Span (Maybe T.Text) T.Text (V.Vector Accessor) ValueExt
  | EscapeURI Span ValueExt
  deriving (Show, Eq, Read, Generic)

instance J.FromJSON ValueExt where
  parseJSON = \case
  -- TODO: Does this instance make sense given spans?
    J.Null -> pure undefined Null
    J.String s -> pure $ String undefined s
    J.Number i -> pure $ Number undefined i
    J.Bool p -> pure $ Boolean undefined p
    J.Array arr -> Array undefined <$> traverse J.parseJSON arr
    J.Object obj | null obj -> pure $ Null undefined
    J.Object obj -> Object undefined <$> traverse J.parseJSON obj

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
    Gt s _ _ -> s
    Lt s _ _ -> s
    And s _ _ -> s
    Or s _ _ -> s
    Member s _ _ -> s
    Range s _ _ _ _ -> s
    EscapeURI s _ -> s

instance Located Accessor where
  locate = \case
    Obj s _ -> s
    Arr s _ -> s
