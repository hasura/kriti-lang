module Kriti.Parser
  ( module L,
    module G,
    module M,
    module S,
    module T,
    lexer,
    parser,
    templateParser'
  )
where

import qualified Data.ByteString as B
import Kriti.Parser.Grammar as G hiding (parser)
import qualified Kriti.Parser.Grammar as GG
import Kriti.Parser.Lexer as L hiding (lexer)
import qualified Kriti.Parser.Lexer as LL
import Kriti.Parser.Monad as M
import Kriti.Parser.Spans as S
import Kriti.Parser.Token as T

lexer :: B.ByteString -> Either ParseError [T.Token]
lexer bs = M.runParser [] bs LL.lexer

parser :: B.ByteString -> Either ParseError T.ValueExt
parser bs = M.runParser [] bs $ do
  toks <- LL.lexer
  GG.parser toks

templateParser' :: B.ByteString -> Either ParseError T.ValueExt
templateParser' bs = M.runParser [] bs $ do
  toks <- LL.lexer
  GG.templateParser toks
