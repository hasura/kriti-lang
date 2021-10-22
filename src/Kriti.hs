module Kriti (RenderedError (..), ErrorCode (..), SourcePosition (..), ValueExt (..), runKriti) where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.Text as T
import Kriti.Error
import Kriti.Eval
import Kriti.Lexer
import Kriti.Parser (ValueExt (..), parser)

runKriti :: T.Text -> [(T.Text, J.Value)] -> Either RenderedError J.Value
runKriti template source = do
  lexemes <- pure $ lexer $ T.unpack template
  template' <- first render $ parser $ fmap teType lexemes
  first render $ runEval template' source
