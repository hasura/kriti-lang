module Kriti.Error where

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Kriti.Parser.Spans as S
import Prettyprinter

data ErrorCode
  = InvalidPathCode
  | TypeErrorCode
  | RangeErrorCode
  | ParseErrorCode
  | LexErrorCode
  deriving (Show)

instance Pretty ErrorCode where
  pretty = \case
    InvalidPathCode -> "Invalid Path"
    TypeErrorCode -> "Type Error"
    RangeErrorCode -> "Out of Range Error"
    ParseErrorCode -> "Parse Error"
    LexErrorCode -> "Lex Error"

data RenderedError = RenderedError {_code :: ErrorCode, _message :: T.Text, _span :: S.Span}
  deriving Show

instance J.ToJSON RenderedError where
  toJSON (RenderedError ec msg span') =
    let (S.AlexSourcePos startLine startCol) = S.start span'
        (S.AlexSourcePos endLine endCol) = S.end span'
     in J.object
          [ "error_code" J..= J.String (T.pack $ show ec),
            "message" J..= J.String msg,
            "source_position"
              J..= J.object
                [ "start_line" J..= startLine,
                  "start_column" J..= startCol,
                  "end_line" J..= endLine,
                  "end_column" J..= endCol
                ]
          ]

class RenderError e where
  render :: e -> RenderedError

instance Pretty RenderedError where
   pretty RenderedError {..} = 
     vsep [ pretty _code <> colon
          , indent 2 $ pretty _message
          ]
