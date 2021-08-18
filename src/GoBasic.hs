module GoBasic (GoBasicErr(..), ValueExt(..), runGoBasic)  where

import GoBasic.Eval
import GoBasic.Lexer (lexer)
import GoBasic.Parser (ValueExt(..), parse)

import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Aeson as J
import qualified Text.Parsec as P

data GoBasicErr =
    ParseError P.ParseError
  | EvalError String
  deriving (Show)

runGoBasic :: Text -> [(Text, J.Value)] -> Either GoBasicErr J.Value
runGoBasic template source = do
  template' <- first ParseError $ parse $ lexer template
  first EvalError $ runEval template' source
