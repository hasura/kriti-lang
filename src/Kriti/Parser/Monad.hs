module Kriti.Parser.Monad where

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.UTF8 as UTFBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import qualified Kriti.Error as E
import Kriti.Parser.Spans
import Kriti.Parser.Token

data ParserState = ParserState
  { parseInput :: {-# UNPACK #-} !AlexInput
  , parseStartCodes :: {-# UNPACK #-} !(NE.NonEmpty Int)
  , parseSpan :: !Span
  }

initState :: [Int] -> B.ByteString -> ParserState
initState codes bs = ParserState
  { parseInput      = AlexInput (AlexSourcePos 0 1) '\n' bs []
  , parseStartCodes = NE.fromList (codes ++ [0])
  , parseSpan       = Span (AlexSourcePos 0 1) (AlexSourcePos 0 1)
  }
  
newtype Parser a = Parser { unParser :: StateT ParserState (Except ParseError) a }
    deriving newtype (Functor, Applicative, Monad, MonadState ParserState, MonadError ParseError)

runParser :: [Int] -> B.ByteString -> Parser a -> Either ParseError a
runParser codes bs p = runExcept $ evalStateT (unParser p) (initState codes bs)

------------------------
--- State Management ---
------------------------

{-# INLINE advance #-}
advance :: AlexInput -> Parser ()
advance input@AlexInput{ lexPos } = do
  modify' $ \s ->
    s { parseInput = input
      , parseSpan = Span (end $ parseSpan s) lexPos
      }

{-# INLINE setInput #-}
setInput :: AlexInput -> Parser ()
setInput input = modify' $ \s -> s { parseInput = input }

{-# INLINE getInput #-}
getInput :: Parser AlexInput
getInput = gets parseInput

{-# INLINE getParseColumn #-}
getParseColumn :: Parser Int
getParseColumn = gets (col . lexPos . parseInput)

{-# INLINE location #-}
location :: Parser Span
location = gets parseSpan

{-# INLINE located #-}
located :: a -> Parser (Loc a)
located a = do
  sp <- location
  pure $ Loc sp a

-------------------
--- Start Codes ---
-------------------

-- | Get the current start code.
startCode :: Parser Int
startCode = gets (NE.head . parseStartCodes)

-- | Push a new start code to the stack.
pushStartCode :: Int -> Parser ()
pushStartCode code = modify' $ \st ->
  st { parseStartCodes = code NE.<| (parseStartCodes st) }

-- | Pop a start code off the stack.
popStartCode :: Parser ()
popStartCode = modify' $ \st ->
  st { parseStartCodes =
       case parseStartCodes st of
         _ NE.:| []     -> 0 NE.:| []
         _ NE.:| (x:xs) -> x NE.:| xs
       }

----------------------
--- Error Handling ---
----------------------

data ParseError =
    EmptyTokenStream Span
  | UnexpectedToken (Loc Token)
  | InvalidLexeme AlexSourcePos B.ByteString
  deriving Show

instance E.RenderError ParseError where
  render (EmptyTokenStream s) =
    E.RenderedError
      { _code = E.ParseErrorCode
      , _message = "ParseError: Empty token stream."
      , _span = s
      }
  render (UnexpectedToken tok) =
    let tok' = serialize $ unLoc tok
        span' = locate tok
    in E.RenderedError
      { _code = E.ParseErrorCode
      , _message = "ParseError: Unexpected token '" <> tok' <> "'."
      , _span = span'
      }
  render (InvalidLexeme start inp) =
    E.RenderedError
        { _code = E.LexErrorCode
        , _message = "LexError: Invalid Lexeme: '" <> TE.decodeUtf8 inp <> "'"
        , _span = Span start (overCol (+ (B.length inp)) start)
        }

parseError :: ParseError -> Parser a
parseError err = throwError err

--------------
--- Tokens ---
--------------

-- | Construct a Token from the matched Text and the current `parseSpan`.
{-# INLINE token #-}
token :: (Loc T.Text -> Token) -> B.ByteString -> Parser Token
token k bs = k <$> located (TE.decodeUtf8 bs)

-- | Construct a `(TokenSymbol (Loc _))` using the current `parseSpan`
-- to construct the `Loc _`.
{-# INLINE symbol #-}
symbol :: Symbol -> B.ByteString -> Parser Token
symbol sym _ = do
  sp <- location
  pure $ TokSymbol $ Loc sp sym

-----------------------
--- Alex Primitives ---
-----------------------

data AlexInput = AlexInput
  { lexPos :: AlexSourcePos
  , lexPrevChar   :: Char
  , lexBytes :: B.ByteString
  -- ^ current input bytestring
  , lexCharBytes :: [Word8]
  -- ^ remaining bytes in current character
  }

{-# INLINE nextLine #-}
nextLine :: B.ByteString -> AlexInput -> AlexInput
nextLine rest AlexInput{..} = AlexInput
  { lexPos = lexPos { line = line lexPos + 1, col = 1 }
  , lexPrevChar = '\n'
  , lexBytes = rest
  , lexCharBytes = []
  }

{-# INLINE nextCol #-}
nextCol :: Char -> B.ByteString -> AlexInput -> AlexInput
nextCol c rest AlexInput{..} = AlexInput
  { lexPos = lexPos { col = col lexPos + 1 }
  , lexPrevChar = c
  , lexBytes = rest
  , ..
  }

{-# INLINE popBufferedBytes #-}
popBufferedBytes :: AlexInput -> Maybe (Word8, AlexInput)
popBufferedBytes AlexInput{..} = 
  case lexCharBytes of
    [] -> Nothing
    (b : bs) -> Just (b, AlexInput { lexCharBytes = bs, .. })

{-# INLINE bufferBytes #-}
bufferBytes :: Char -> [Word8] -> B.ByteString -> AlexInput -> AlexInput
bufferBytes c bytes rest AlexInput{..} = AlexInput
  { lexPrevChar = c
  , lexBytes = rest
  , lexCharBytes = bytes
  , ..
  }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{..} =
  case popBufferedBytes input of
    Nothing -> advance' <$> UTFBS.uncons lexBytes
    ok      -> ok
  where
    advance' :: (Char, B.ByteString) -> (Word8, AlexInput)
    advance' ('\n', rest) = (B.c2w '\n', nextLine rest input)
    advance' (c, rest)   =
      case UTF8.encodeChar c of
        [b]    -> (b, nextCol c rest input)
        (b:bs) -> (b, bufferBytes c bs rest input)
        []     -> error "The impossible happened! A Char decoded to 0 bytes."

alexPrevInputChar :: AlexInput -> Char
alexPrevInputChar = lexPrevChar

{-# INLINE slice #-}
slice :: Int -> AlexInput -> B.ByteString
slice n AlexInput{..} = B.take n lexBytes
