module Kriti (RenderedError (..), ErrorCode (..), AlexSourcePos (..), ValueExt (..), runKriti) where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Kriti.Error
import Kriti.Eval
import Kriti.Parser
import Kriti.Parser.Spans
import Kriti.Parser.Token

runKriti :: B.ByteString -> [(T.Text, J.Value)] -> Either RenderedError J.Value
runKriti template source = do
  template' <- first render $ parser template
  first render $ runEval template' source
