module Main where

----------------------------------------------------------------------

import Control.Applicative
import Control.Monad.State
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Char as Char
import Data.List (isPrefixOf)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Kriti
import Kriti.Aeson.Pretty ()
import Kriti.CustomFunctions (basicFuncMap)
import Prettyprinter
import System.Console.Repline
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

----------------------------------------------------------------------

main :: IO ()
main =
  flip evalStateT mempty $
    evalReplOpts $
      ReplOpts
        { banner = const $ pure "> ",
          command = command',
          options = options',
          prefix = Just ':',
          multilineCommand = Just "paste",
          tabComplete = Prefix (wordCompleter defaultCompleter) prefixCompleters,
          initialiser = liftIO $ putStrLn "Kriti Lang, version 0.3.2: github.com/hasura/kriti-lang/ :? for help",
          finaliser = liftIO $ putStrLn "Goodbye!" >> pure Exit
        }

options' :: [(String, String -> HaskelineT (StateT (Map Text J.Value) IO) ())]
options' =
  [ ("?", \_ -> helpCommand),
    ("let", \args -> letCommand args),
    ("dump", \_ -> dumpCommand)
  ]

----------------------------------------------------------------------

command' :: String -> HaskelineT (StateT (Map Text J.Value) IO) ()
command' input = do
  ctx <- gets Map.toList
  case runKritiWith (Text.pack input) ctx basicFuncMap of
    Left err -> liftIO $ print $ pretty err
    Right json -> liftIO $ print $ pretty json

prefixCompleters :: MonadIO m => [(String, CompletionFunc m)]
prefixCompleters = [(":let", fileCompleter)]

defaultCompleter :: MonadState (Map Text J.Value) m => WordCompleter m
defaultCompleter n = do
  ctx <- gets (fmap (Text.unpack . fst) . Map.toList)
  return $ filter (isPrefixOf n) ctx

----------------------------------------------------------------------

helpCommand :: HaskelineT (StateT (Map Text J.Value) IO) ()
helpCommand = liftIO $ putStrLn "Help **TODO**"

----------------------------------------------------------------------

dumpCommand :: HaskelineT (StateT (Map Text J.Value) IO) ()
dumpCommand = do
  ctx <- get
  void $
    liftIO $
      flip Map.traverseWithKey ctx $ \bndr json -> do
        print $ pretty bndr <+> "=" <+> pretty json

----------------------------------------------------------------------

letCommand :: String -> HaskelineT (StateT (Map Text J.Value) IO) ()
letCommand args = do
  case parseArgs args of
    Nothing -> liftIO $ print $ prettyParseError "Unexpected Token" args
    Just (bndr, arg) -> do
      jsonM <- liftIO $ loadFile arg <|> loadJSON arg
      case jsonM of
        Left err -> liftIO $ print $ prettyParseError err arg
        Right json -> modify $ Map.insert (Text.pack bndr) json

loadFile :: String -> IO (Either String J.Value)
loadFile path =
  let trim = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace
   in J.eitherDecode @J.Value <$> BL.readFile (trim path)

loadJSON :: String -> IO (Either String J.Value)
loadJSON bs = pure $ J.eitherDecode @J.Value (Char8.pack bs)

parseArgs :: String -> Maybe (String, String)
parseArgs = listToMaybe . fmap fst . ReadP.readP_to_S argParser

-- TODO: Replace with a parsing library so we can get spans for error
-- messages.
argParser :: ReadP (String, String)
argParser = do
  bndr <- ReadP.many1 $ ReadP.satisfy (/= ' ')
  ReadP.skipSpaces
  void $ ReadP.char '='
  ReadP.skipSpaces
  val <- ReadP.munch (const True)
  ReadP.eof
  pure (bndr, val)

prettyParseError :: String -> String -> Doc ann
prettyParseError msg src =
  vsep
    [ "Parse Error:",
      indent 2 $ pretty msg,
      indent 4 $ "|",
      indent 4 $ "|" <+> pretty src,
      indent 4 $ "|"
    ]
