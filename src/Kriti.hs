module Kriti (RenderedError (..), ErrorCode (..), SourcePosition (..), ValueExt (..), runKriti) where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.Text as T
import Kriti.Error
import Kriti.Eval
import Kriti.Lexer (lexer)
import Kriti.Parser (ValueExt (..), parser)

runKriti :: T.Text -> [(T.Text, J.Value)] -> Either RenderedError J.Value
runKriti template source = do
  lexemes <- pure $ lexer $ T.unpack template
  template' <- pure $ parser lexemes
  first render $ runEval template' source
