module Main where

----------------------------------------------------------------------

import Control.Monad.State
import qualified Data.Aeson as J
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Kriti
import Prettyprinter
import System.Console.Repline
import qualified Data.ByteString.Lazy as BL

----------------------------------------------------------------------

main :: IO ()
main =
  flip evalStateT [] $
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

cmd :: String -> HaskelineT (StateT [(Text, J.Value)] IO) ()
cmd input = do
  ctx <- get
  case runKriti (Text.pack input) ctx of
    Left err -> liftIO $ print $ pretty err
    Right json -> liftIO $ Char8.putStrLn $ encodePretty json

completer :: Monad m => WordCompleter m
completer n = do
  let names = []
  return $ filter (isPrefixOf n) names

opts :: [(String, String -> HaskelineT (StateT [(Text, J.Value)] IO) ())]
opts =
  [ ("?", \_ -> helpMessage),
    ("load", \file -> loadFile $ words file),
    ("l", \file -> loadFile $ words file)
  ]

helpMessage :: HaskelineT (StateT [(Text, J.Value)] IO) ()
helpMessage = liftIO $ putStrLn "Help"

loadFile :: [String] -> HaskelineT (StateT [(Text, J.Value)] IO) ()
loadFile [bndr, path] = do
  json <- liftIO $ J.decode @J.Value <$> BL.readFile path
  case json of
    Nothing -> fail "oops"
    Just json' -> modify $ ((Text.pack bndr, json') :)
loadFile _ = fail "oops"
