module Kriti (RenderedError (..), ErrorCode (..), AlexSourcePos (..), ValueExt (..), runKriti) where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Kriti.Error
import Kriti.Eval
import Kriti.Parser

runKriti :: T.Text -> [(T.Text, J.Value)] -> Either RenderedError J.Value
runKriti template source = do
  template' <- first render $ parser $ T.encodeUtf8 template
  first render $ runEval template' source
