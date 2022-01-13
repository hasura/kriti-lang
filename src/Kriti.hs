module Kriti
  ( SerializedError (..),
    ErrorCode (..),
    AlexSourcePos (..),
    ValueExt (..),
    parser,
    renderPretty,
    runKriti,
    runKritiBS,
    runEval,
  )
where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Kriti.Error
import Kriti.Eval
import Kriti.Parser

-- | Entry point for Kriti when given a template as 'Text'.
runKriti :: T.Text -> [(T.Text, J.Value)] -> Either SerializedError J.Value
runKriti templateSrc source = do
  let templateSrc' =  T.encodeUtf8 templateSrc
  template' <- first serialize $ parser templateSrc'
  first serialize $ runEval templateSrc' template' source

-- | Entry point for Kriti when given a template as 'ByteString'.
runKritiBS :: B.ByteString -> [(T.Text, J.Value)] -> Either SerializedError J.Value
runKritiBS templateSrc json = do
  ast <- first serialize $ parser templateSrc
  first serialize $ runEval templateSrc ast json
