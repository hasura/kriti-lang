module Kriti
  ( SerializedError (..),
    ErrorCode (..),
    AlexSourcePos (..),
    ValueExt (..),
    renderPretty,
    runKriti,
    runKritiBS,
    runKritiWith,
    runKritiBSWith,
  )
where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.HashMap.Internal as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Kriti.Error
import Kriti.Eval
import Kriti.Parser
import Prettyprinter (Pretty (..))

data KritiError = KritiParseError ParseError | KritiEvalError EvalError

instance Pretty KritiError where
  pretty = \case
    KritiParseError err -> pretty err
    KritiEvalError err -> pretty err

instance SerializeError KritiError where
  serialize = \case
    KritiParseError err -> serialize err
    KritiEvalError err -> serialize err

-- | Entry point for Kriti when given a template as 'Text'.
runKriti :: T.Text -> [(T.Text, J.Value)] -> Either KritiError J.Value
runKriti templateSrc source = do
  let templateSrc' = T.encodeUtf8 templateSrc
  template' <- first KritiParseError $ parser templateSrc'
  first KritiEvalError $ runEval templateSrc' template' source

runKritiWith :: T.Text -> [(T.Text, J.Value)] -> Map.HashMap T.Text (J.Value -> Either CustomFunctionError J.Value) -> Either KritiError J.Value
runKritiWith templateSrc source funcMap = do
  let templateSrc' = T.encodeUtf8 templateSrc
  template' <- first KritiParseError $ parser templateSrc'
  first KritiEvalError $ runEvalWith templateSrc' template' source funcMap

-- | Entry point for Kriti when given a template as
-- 'ByteString'. Caller must ensure that the input is valid UTF8
-- encoded.
runKritiBS :: B.ByteString -> [(T.Text, J.Value)] -> Either KritiError J.Value
runKritiBS templateSrc json = do
  ast <- first KritiParseError $ parser templateSrc
  first KritiEvalError $ runEval templateSrc ast json

-- | The same as 'runKritiBS' but allows use of a custom function map.
runKritiBSWith :: B.ByteString -> [(T.Text, J.Value)] -> Map.HashMap T.Text (J.Value -> Either CustomFunctionError J.Value) -> Either KritiError J.Value
runKritiBSWith templateSrc json funcMap = do
  ast <- first KritiParseError $ parser templateSrc
  first KritiEvalError $ runEvalWith templateSrc ast json funcMap