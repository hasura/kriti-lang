{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kriti.Parser.Token where

import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Generics
import qualified Kriti.Aeson.Compat as Compat
import Kriti.Parser.Spans
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

-- | The type of non literal/identifer symbols extracted from
-- source. This type is factored out of `Token` for clarity.
data Symbol
  = SymBling
  | SymColon
  | SymDot
  | SymComma
  | SymQuestionMark
  | SymDoubleQuestionMark
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

serializeToken :: Token -> T.Text
serializeToken = \case
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
  TokSymbol (Loc _ SymQuestionMark) -> "?"
  TokSymbol (Loc _ SymDoubleQuestionMark) -> "??"
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

-- | The elif conditional expression
data Elif = Elif Span ValueExt ValueExt
  deriving (Show, Eq, Read, Generic)

instance Pretty Elif where
  pretty (Elif _ c t) =
    vsep
      [ "{{" <+> "elif" <+> pretty c <+> "}}",
        indent 2 $ pretty t
      ]

-- | The Kriti AST type. Kriti templates are parsed into `ValueExt`
-- terms which are then evaluated and converted into Aeson `Value`
-- terms.
data ValueExt
  = Object Span (Compat.Object ValueExt)
  | Array Span (V.Vector ValueExt)
  | String Span T.Text
  | Number Span Scientific
  | Boolean Span Bool
  | Null Span
  | StringTem Span (V.Vector ValueExt)
  | Var Span T.Text
  | RequiredFieldAccess Span ValueExt (Either T.Text ValueExt)
  | OptionalFieldAccess Span ValueExt [Either T.Text ValueExt]
  | Iff Span ValueExt ValueExt (V.Vector Elif) ValueExt
  | Eq Span ValueExt ValueExt
  | NotEq Span ValueExt ValueExt
  | Gt Span ValueExt ValueExt
  | Gte Span ValueExt ValueExt
  | Lt Span ValueExt ValueExt
  | Lte Span ValueExt ValueExt
  | And Span ValueExt ValueExt
  | Or Span ValueExt ValueExt
  | In Span ValueExt ValueExt
  | Defaulting Span ValueExt ValueExt
  | Ternary Span ValueExt ValueExt ValueExt
  | Range Span (Maybe T.Text) T.Text ValueExt ValueExt
  | Function Span T.Text ValueExt
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
    Var s _ -> s
    RequiredFieldAccess s _ _ -> s
    OptionalFieldAccess s _ _ -> s
    Iff s _ _ _ _ -> s
    Eq s _ _ -> s
    NotEq s _ _ -> s
    Gt s _ _ -> s
    Lt s _ _ -> s
    Gte s _ _ -> s
    Lte s _ _ -> s
    And s _ _ -> s
    Or s _ _ -> s
    In s _ _ -> s
    Defaulting s _ _ -> s
    Ternary s _ _ _ -> s
    Range s _ _ _ _ -> s
    Function s _ _ -> s

instance Pretty ValueExt where
  pretty = \case
    Object _ km ->
      let p :: (T.Text, ValueExt) -> Doc a
          p (k, v) = pretty k <> colon <+> pretty v <> comma
       in braces $ braces $ foldMap p $ Compat.toList km
    Array _ vec -> pretty $ V.toList vec
    String _ txt -> dquotes (pretty txt)
    Number _ sci -> pretty $ show sci
    Boolean _ b -> pretty b
    Null _ -> "null"
    StringTem _ vec ->
      dquotes $
        vec & foldMap \case
          String _ txt -> pretty txt
          t1 -> "{{" <+> pretty t1 <+> "}}"
    Var _ t -> pretty t
    RequiredFieldAccess _ t1 field -> pretty t1 <> "." <> either pretty pretty field
    OptionalFieldAccess _ t1 fields -> pretty t1 <> "?." <> foldMap ((<> ".") . either pretty pretty) fields
    Iff _ p t1 elifs t2 ->
      vsep $
        [ "{{" <+> "if" <+> pretty p <+> "}}",
          indent 2 $ pretty t1
        ]
          <> map pretty (V.toList elifs)
          <> [ indent 2 $ pretty t1,
               "{{" <+> "else" <+> "}}",
               indent 2 $ pretty t2,
               "{{" <+> "end" <+> "}}"
             ]
    Eq _ t1 t2 -> pretty t1 <+> equals <+> pretty t2
    NotEq _ t1 t2 -> pretty t1 <+> "!=" <+> pretty t2
    Gt _ t1 t2 -> pretty t1 <+> ">" <+> pretty t2
    Lt _ t1 t2 -> pretty t1 <+> "<" <+> pretty t2
    Gte _ t1 t2 -> pretty t1 <+> ">=" <+> pretty t2
    Lte _ t1 t2 -> pretty t1 <+> "<=" <+> pretty t2
    And _ t1 t2 -> pretty t1 <+> "&&" <+> pretty t2
    Or _ t1 t2 -> pretty t1 <+> "||" <+> pretty t2
    In _ t1 t2 -> pretty t1 <+> "in" <+> pretty t2
    Defaulting _ t1 t2 -> pretty t1 <+> "??" <+> pretty t2
    Ternary _ p t1 t2 -> pretty p <+> "?" <+> pretty t1 <+> ":" <+> pretty t2
    Range _ i bndr xs t1 ->
      vsep
        [ "{{" <+> "range" <+> pretty i <> comma <+> pretty bndr <+> colon <> equals <+> pretty xs <+> "}}",
          indent 2 $ pretty t1,
          "{{" <+> "end" <+> "}}"
        ]
    Function _ n t1 -> "{{" <+> pretty n <+> " {{" <+> pretty t1 <+> "}} }}"

renderDoc :: Doc ann -> T.Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

renderPretty :: (Pretty a) => a -> T.Text
renderPretty = renderDoc . pretty

renderVect :: (Pretty a) => V.Vector a -> T.Text
renderVect = renderDoc . foldMap pretty

renderBL :: BL.ByteString -> T.Text
renderBL = TE.decodeUtf8 . BL.toStrict
