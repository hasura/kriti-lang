module Kriti (KritiErr(..), ValueExt(..), runKriti)  where

import Kriti.Eval
import Kriti.Lexer (lexer)
import Kriti.Parser (ValueExt(..), parse)

import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Aeson as J
import qualified Text.Parsec as P

data KritiErr =
    ParseError P.ParseError
  | EvalError String
  deriving Show

runKriti :: Text -> [(Text, J.Value)] -> Either KritiErr J.Value
runKriti template source = do
  template' <- first ParseError $ parse $ lexer template
  first EvalError $ runEval template' source
