module Kriti.Parser
  ( module L,
    module G,
    module M,
    module S,
    module T,
    lexer,
    parser,
  )
where

--------------------------------------------------------------------------------

import Data.ByteString qualified as B
import Kriti.Parser.Grammar as G hiding (parser)
import Kriti.Parser.Grammar qualified as GG
import Kriti.Parser.Lexer as L hiding (lexer)
import Kriti.Parser.Lexer qualified as LL
import Kriti.Parser.Monad as M
import Kriti.Parser.Spans as S
import Kriti.Parser.Token as T

--------------------------------------------------------------------------------

lexer :: B.ByteString -> Either ParseError [T.Token]
lexer bs = M.runParser [] bs LL.lexer

parser :: B.ByteString -> Either ParseError T.ValueExt
parser bs = M.runParser [] bs $ do
  toks <- LL.lexer
  GG.parser toks
