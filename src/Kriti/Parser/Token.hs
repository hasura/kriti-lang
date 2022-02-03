{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Kriti.Parser.Token where

import qualified Data.ByteString.Lazy as BL
import Data.Fix
import Data.Function ((&))
import Data.Functor.Classes.Generic
import Data.HashMap.Strict as M
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Generics
import Kriti.Parser.Spans
import Prettyprinter hiding (align)
import Prettyprinter.Render.Text (renderStrict)
import Data.Functor.Sum
import Data.Functor.Classes

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

data JsonF r
  = -- | Core Aeson Terms
    Object Span (M.HashMap T.Text r)
  | Array Span (V.Vector r)
  | String Span T.Text
  | Number Span Scientific
  | Boolean Span Bool
  | Null Span
  deriving (Show, Eq, Read, Functor, Generic, Generic1)

instance Eq1 JsonF where
  liftEq = liftEqDefault

instance Show1 JsonF where
  liftShowsPrec = liftShowsPrecDefault

instance Read1 JsonF where
  liftReadsPrec = liftReadsPrecDefault

-- | The Kriti AST type. Kriti templates are parsed into `ValueExt`
-- terms which are then evaluated and converted into Aeson `Value`
-- terms.
data ValueExtF r
  = StringTemF Span (V.Vector r)
  | PathF Span (V.Vector Accessor)
  | IffF Span r r r
  | EqF Span r r
  | NotEqF Span r r
  | GtF Span r r
  | GteF Span r r
  | LtF Span r r
  | LteF Span r r
  | AndF Span r r
  | OrF Span r r
  | InF Span r r
  | NotF Span r
  | RangeF Span (Maybe T.Text) T.Text (V.Vector Accessor) r
  | EscapeURIF Span r
  deriving (Show, Eq, Read, Functor, Generic, Generic1)

instance Eq1 ValueExtF where
  liftEq = liftEqDefault

instance Show1 ValueExtF where
  liftShowsPrec = liftShowsPrecDefault

instance Read1 ValueExtF where
  liftReadsPrec = liftReadsPrecDefault

mkKriti :: ValueExtF Expr -> Expr
mkKriti = Fix . InR

mkJson :: JsonF Expr -> Expr
mkJson = Fix . InL

--pattern StringTem :: Span -> (V.Vector (ValueExtF r)) -> ValueExtF r
--pattern StringTem = _
  
type ExprF = Sum JsonF ValueExtF
type Expr = Fix ExprF
  
instance Located Expr where
  locate (Fix expr) = case expr of
    InL json -> locate json
    InR kriti -> locate kriti

instance Located (JsonF r) where
  locate = \case
    Object s _ -> s
    Array s _ -> s
    String s _ -> s
    Number s _ -> s
    Boolean s _ -> s
    Null s -> s
   
instance Located (ValueExtF r) where
  locate = \case
    StringTemF s _ -> s
    PathF s _ -> s
    IffF s _ _ _ -> s
    EqF s _ _ -> s
    NotEqF s _ _ -> s
    GtF s _ _ -> s
    LtF s _ _ -> s
    GteF s _ _ -> s
    LteF s _ _ -> s
    AndF s _ _ -> s
    OrF s _ _ -> s
    InF s _ _ -> s
    NotF s _ -> s
    RangeF s _ _ _ _ -> s
    EscapeURIF s _ -> s

instance Located Accessor where
  locate = \case
    Obj s _ _ -> s
    Arr s _ -> s

instance Pretty r => Pretty (JsonF r) where
  pretty = \case
    Object _ km ->
      let p :: (T.Text, r) -> Doc a
          p (k, v) = pretty k <> colon <+> pretty v <> comma
       in braces $ braces $ foldMap p $ M.toList km
    Array _ vec -> pretty (V.toList vec)
    String _ txt -> dquotes (pretty txt)
    Number _ sci -> pretty (show sci) -- TODO: correct scientific printing
    Boolean _ b -> pretty b
    Null _ -> "null"

instance Pretty r => Pretty (ValueExtF r) where
  pretty = \case
    StringTemF _ vec ->
      dquotes $ vec & foldMap pretty
    PathF _ vec -> surround (foldMap pretty vec) "{{ " " }}"
    IffF _ p t1 t2 ->
      vsep
        [ "{{" <+> "if" <+> pretty p <+> "}}",
          indent 2 $ pretty t1,
          "{{" <+> "else" <+> "}}",
          indent 2 $ pretty t2,
          "{{" <+> "end" <+> "}}"
        ]
    EqF _ t1 t2 -> pretty t1 <+> equals <+> pretty t2
    NotEqF _ t1 t2 -> pretty t1 <+> "!=" <+> pretty t2
    GtF _ t1 t2 -> pretty t1 <+> ">" <+> pretty t2
    LtF _ t1 t2 -> pretty t1 <+> "<" <+> pretty t2
    GteF _ t1 t2 -> pretty t1 <+> ">=" <+> pretty t2
    LteF _ t1 t2 -> pretty t1 <+> "<=" <+> pretty t2
    AndF _ t1 t2 -> pretty t1 <+> "&&" <+> pretty t2
    OrF _ t1 t2 -> pretty t1 <+> "||" <+> pretty t2
    InF _ t1 t2 -> pretty t1 <+> "in" <+> pretty t2
    NotF _ t1 -> "not" <+> pretty t1
    RangeF _ i bndr xs t1 ->
      vsep
        [ "{{" <+> "range" <+> pretty i <> comma <+> pretty bndr <+> colon <> equals <+> foldMap pretty xs <+> "}}",
          indent 2 $ pretty t1,
          "{{" <+> "end" <+> "}}"
        ]
    EscapeURIF _ t1 -> "{{" <+> "escapeUri" <+> pretty t1 <+> "}}"

instance Pretty Expr where
  pretty (Fix (InL json)) = pretty json
  pretty (Fix (InR kriti)) = pretty kriti

renderDoc :: Doc ann -> T.Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

renderPretty :: Pretty a => a -> T.Text
renderPretty = renderDoc . pretty

renderVect :: Pretty a => V.Vector a -> T.Text
renderVect = renderDoc . foldMap pretty

renderBL :: BL.ByteString -> T.Text
renderBL = TE.decodeUtf8 . BL.toStrict
