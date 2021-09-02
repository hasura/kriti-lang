{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE NamedFieldPuns #-}

module Kriti.Error (
  Error(..),
  ErrorCode(..),
  SourcePosition(..),
  Span(..),
  ToError(..),
  TryFromError(..),
  fromSourcePos,
  incCol
) where

import qualified Data.Aeson    as J
import           Data.Dynamic  (Dynamic, fromDynamic)
import           Data.Typeable (Typeable)
import           Data.Maybe    (catMaybes)
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Text.Parsec   as P

type SourceName = String
type Line = Int
type Column = Int

-- | Isomorphic to Parsec's SourcePos. We need this because our golden
-- tests require Read instances for all our types.
data SourcePosition = SourcePosition {
  _sourceName :: SourceName,
  _line :: Line,
  _column :: Column
}
  deriving stock (Eq, Read, Show)

-- | TODO: Documentation.
fromSourcePos :: P.SourcePos -> SourcePosition
fromSourcePos pos =
  SourcePosition (P.sourceName pos) (P.sourceLine pos) (P.sourceColumn pos)

-- | TODO: Documentation.
incCol :: Int -> SourcePosition -> SourcePosition
incCol i sourcePos@(SourcePosition {_column}) = sourcePos{_column = i + _column}

-- | TODO: Documentation.
--
-- TODO: Better naming.
data Span = Span {
  span_fst :: !SourcePosition,
  span_snd :: !(Maybe SourcePosition)
}
  deriving stock (Eq, Read, Show)

data ErrorCode =
    InvalidPathCode
  | TypeErrorCode
  | RangeErrorCode
  | ParseErrorCode
  deriving stock (Show)

-- | TODO: Documentation.
--
-- TODO: Better naming.
data Error = Error {
  _code :: !ErrorCode,
  _message :: !Text,
  _span :: !Span,
  _innerError :: !Dynamic
}
  deriving stock (Show)

instance J.ToJSON Error where
  toJSON (Error ec msg errSpan _innerError) =
    let (SourcePosition _ startLine startCol) = span_fst errSpan
        mEndLine = _line <$> span_snd errSpan
        mEndCol = _column <$> span_snd errSpan
        sourcePosObj = J.object $ [
            "start_line" J..= startLine,
            "start_col" J..= startCol
          ] <> catMaybes [
            ("end_line" J..=) <$> mEndLine,
            ("end_column" J..=) <$> mEndCol
          ]
    in
      J.object [
        "error_code" J..= J.String (T.pack $ show ec),
        "message" J..= J.String msg,
        "source_position" J..= sourcePosObj
      ]

-- | TODO: Documentation.
class ToError e where
  toError :: e -> Error

-- | TODO: Documentation.
class TryFromError e where
  tryFromError :: Typeable e => Error -> Maybe e
  tryFromError Error{_innerError} = fromDynamic _innerError
