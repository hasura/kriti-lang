module GoBasic (GoBasicErr(..), runGoBasic)  where

import GoBasic.Eval
import GoBasic.Lexer (lexer)
import GoBasic.Parser (parse)

import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Aeson as J
import qualified Text.Parsec as P

data GoBasicErr =
    ParseError P.ParseError
  | EvalError String

runGoBasic :: Text -> [(Text, J.Value)] -> Either GoBasicErr J.Value
runGoBasic template source = do
  template' <- first ParseError $ parse $ lexer template
  first EvalError $ runEval template' source
