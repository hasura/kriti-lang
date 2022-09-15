module Main where

----------------------------------------------------------------------

import Control.Applicative
import Control.Monad.State
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.List (isPrefixOf)
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
          command = cmd,
          options = opts,
          prefix = Just ':',
          multilineCommand = Just "paste",
          tabComplete = (Word0 completer),
          initialiser = liftIO $ putStrLn "Kriti Lang, version 0.3.2: github.com/hasura/kriti-lang/ :? for help",
          finaliser = liftIO $ putStrLn "Goodbye!" >> pure Exit
        }

----------------------------------------------------------------------

cmd :: String -> HaskelineT (StateT (Map Text J.Value) IO) ()
cmd input = do
  ctx <- gets Map.toList
  case runKritiWith (Text.pack input) ctx basicFuncMap of
    Left err -> liftIO $ print $ pretty err
    Right json -> liftIO $ print $ pretty json

completer :: MonadState (Map Text J.Value) m => WordCompleter m
completer n = do
  ctx <- gets (fmap (Text.unpack . fst) . Map.toList)
  return $ filter (isPrefixOf n) ctx

opts :: [(String, String -> HaskelineT (StateT (Map Text J.Value) IO) ())]
opts =
  [ ("?", \_ -> helpMessage),
    ("let", \args -> letCommand args),
    ("dump", \_ -> printState)
  ]

helpMessage :: HaskelineT (StateT (Map Text J.Value) IO) ()
helpMessage = liftIO $ putStrLn "Help"

letCommand :: String -> HaskelineT (StateT (Map Text J.Value) IO) ()
letCommand args = do
  case parseArgs args of
    Nothing -> liftIO $ putStrLn "parse error **TODO**"
    Just (bndr, arg) -> do
      jsonM <- liftIO $ loadFile arg <|> loadJSON arg
      case jsonM of
        Nothing -> liftIO $ putStrLn "Invalid JSON Expression **TODO**"
        Just json -> modify $ Map.insert (Text.pack bndr) json

loadFile :: String -> IO (Maybe J.Value)
loadFile path = J.decode @J.Value <$> BL.readFile path

loadJSON :: String -> IO (Maybe J.Value)
loadJSON bs = pure $ J.decode @J.Value (Char8.pack bs)

printState :: HaskelineT (StateT (Map Text J.Value) IO) ()
printState = do
  ctx <- get
  void $
    liftIO $
      flip Map.traverseWithKey ctx $ \bndr json -> do
        print $ pretty bndr <+> "=" <+> pretty json

parseArgs :: String -> Maybe (String, String)
parseArgs = listToMaybe . fmap fst . ReadP.readP_to_S argParser

argParser :: ReadP (String, String)
argParser = do
  bndr <- ReadP.many1 $ ReadP.satisfy (/= ' ')
  ReadP.skipSpaces
  void $ ReadP.char '='
  ReadP.skipSpaces
  val <- ReadP.munch (const True)
  ReadP.eof
  pure (bndr, val)
