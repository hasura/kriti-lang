module Kriti (RenderedError(..), ErrorCode(..), SourcePosition(..), ValueExt(..), runKriti)  where


import           Data.Bifunctor    (first)
import           Kriti.Error
import           Kriti.Eval
import           Kriti.Lexer       (lexer)
import           Kriti.Parser      (ValueExt(..), parser)
import qualified Data.Aeson        as J
import qualified Data.Text         as T

runKriti :: T.Text -> [(T.Text, J.Value)] -> Either RenderedError J.Value
runKriti template source = do
  template' <- first render $ parser $ lexer template
  first render $ runEval template' source
