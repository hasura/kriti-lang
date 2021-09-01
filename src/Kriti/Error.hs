module Kriti.Error where

import           Data.Maybe  (catMaybes)
import qualified Data.Aeson  as J
import qualified Data.Text   as T
import qualified Text.Parsec as P

type SourceName = String
type Line = Int
type Column = Int

-- | Isomorphic to Parsec's SourcePos. We need this because our golden
-- tests require Read instances for all our types.
data SourcePosition = SourcePosition { _sourceName :: SourceName, _line :: Line, _column :: Column }
  deriving (Show, Eq, Read)

fromSourcePos :: P.SourcePos -> SourcePosition
fromSourcePos pos = SourcePosition (P.sourceName pos) (P.sourceLine pos) (P.sourceColumn pos)

incCol :: Int -> SourcePosition -> SourcePosition
incCol i (SourcePosition n l c) = SourcePosition n l (i+c)

type Span = (SourcePosition, Maybe SourcePosition)


data ErrorCode = InvalidPathCode | TypeErrorCode | RangeErrorCode | ParseErrorCode
  deriving Show

data RenderedError = RenderedError { _code :: ErrorCode, _message :: T.Text, _span :: Span }
  deriving Show

instance J.ToJSON RenderedError where
  toJSON (RenderedError ec msg span') =
    let (SourcePosition _ startLine startCol) = fst span'
        endLine = _line   <$> snd span'
        endCol  = _column <$> snd span'
    in J.object [ "error_code" J..= J.String (T.pack $ show ec)
                , "message" J..= J.String msg
                , "source_position" J..= J.object (["start_line" J..= startLine
                                                   , "start_column" J..= startCol
                                                   ] <> catMaybes
                                                   [ ("end_line" J..=) <$> endLine
                                                   , ("end_column" J..=) <$> endCol
                                                   ])
                ]

class RenderError e where
  render :: e -> RenderedError
