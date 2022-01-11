module Kriti (RenderedError (..), ErrorCode (..), AlexSourcePos (..), ValueExt (..), runKriti, runKritiTemplate) where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Kriti.Error
import Kriti.Eval
import Kriti.Parser

runKriti :: B.ByteString -> [(T.Text, J.Value)] -> Either RenderedError J.Value
runKriti template source = do
  template' <- first render $ parser template
  first render $ runEval template' source

runKritiTemplate :: B.ByteString -> [(T.Text, J.Value)] -> Either RenderedError J.Value
runKritiTemplate template str = do
  template' <- first render $ templateParser' template
  first render $ runEval template' str
