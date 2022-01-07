module Kriti (SerializedError (..), ErrorCode (..), AlexSourcePos (..), ValueExt (..), runKriti) where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Kriti.Error
import Kriti.Eval
import Kriti.Parser

runKriti :: B.ByteString -> [(T.Text, J.Value)] -> Either SerializedError J.Value
runKriti template source = do
  template' <- first serialize $ parser template
  first serialize $ runEval template' source
