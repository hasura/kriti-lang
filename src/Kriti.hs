module Kriti (
  Error(..),
  ErrorCode(..),
  SourcePosition(..),
  Span(..),
  ValueExt(..),
  runKriti
)  where

import           Data.Bifunctor    (first)
import           Kriti.Error       (Error(..), ErrorCode(..), SourcePosition(..), Span(..), toError)
import           Kriti.Eval        (runEval)
import           Kriti.Lexer       (lexer)
import           Kriti.Parser      (ValueExt(..), parser)
import qualified Data.Aeson        as J
import qualified Data.Text         as T

runKriti :: T.Text -> [(T.Text, J.Value)] -> Either Error J.Value
runKriti template source = do
  template' <- first toError . parser $ lexer template
  first toError $ runEval template' source
