{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kriti.Parser.Token where

import Data.Scientific (Scientific)
import qualified Data.ByteString.Lazy as BL
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
  TokSymbol (Loc _ SymGt) -> ">"
  TokSymbol (Loc _ SymLt) -> "<"
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

-- | Annotates whether the object lookup used '.' or brace syntax for pretty printing.
data ObjAccType = Head | DotAccess | BracketAccess
  deriving (Show, Eq, Read)

-- | Path lookups are represented as a stack of object and array lookups. eg., `Vector Accessor`.
data Accessor = Obj Span T.Text ObjAccType | Arr Span Int
  deriving (Show, Eq, Read)

instance Pretty Accessor where
  pretty = \case
    Obj _ txt DotAccess -> dot <> pretty txt
    Obj _ txt BracketAccess -> brackets (squotes $ pretty txt)
    Obj _ txt Head -> pretty txt
    Arr _ i -> brackets (pretty i)

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
  | Gt Span ValueExt ValueExt
  | Lt Span ValueExt ValueExt
  | And Span ValueExt ValueExt
  | Or Span ValueExt ValueExt
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
    Gt s _ _ -> s
    Lt s _ _ -> s
    And s _ _ -> s
    Or s _ _ -> s
    Member s _ _ -> s
    Range s _ _ _ _ -> s
    EscapeURI s _ -> s

instance Located Accessor where
  locate = \case
    Obj s _ _ -> s
    Arr s _ -> s

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
      dquotes $ flip foldMap vec $ \case
        String _ txt -> pretty txt
        t1 -> "{{" <+> pretty t1 <+> "}}"
    Path _ vec -> surround (foldMap pretty vec) "{{ " " }}" 
    Iff _ p t1 t2 ->
      vsep [ "{{" <+> "if" <+> pretty p <+> "}}",
             indent 2 $ pretty t1,
             "{{" <+> "else" <+> "}}",
             indent 2 $ pretty t2,
             "{{" <+> "end" <+> "}}"
           ]
    Eq _ t1 t2 -> pretty t1 <+> equals <+> pretty t2
    Gt _ t1 t2 -> pretty t1 <+> ">" <+> pretty t2
    Lt _ t1 t2 -> pretty t1 <+> "<" <+> pretty t2
    And _ t1 t2 -> pretty t1 <+> "&&" <+> pretty t2
    Or _ t1 t2 -> pretty t1 <+> "||" <+> pretty t2
    Member _ t1 t2 -> pretty t1 <+> "in" <+> pretty t2
    Range _ i bndr xs t1 ->
      vsep [ "{{" <+> "range" <+> pretty i <> comma <+> pretty bndr <+> colon <> equals <+> foldMap pretty xs <+> "}}",
             indent 2 $ pretty t1,
             "{{" <+> "end" <+> "}}"
           ]
    EscapeURI _ t1 -> "{{" <+> "escapeUri" <+> pretty t1 <+> "}}"

renderDoc :: Doc ann -> T.Text 
renderDoc = renderStrict . layoutPretty defaultLayoutOptions 

renderPretty :: Pretty a => a -> T.Text
renderPretty = renderDoc . pretty

renderVect :: Pretty a => V.Vector a -> T.Text
renderVect = renderDoc . foldMap pretty

renderBL :: BL.ByteString -> T.Text
renderBL = TE.decodeUtf8 . BL.toStrict
