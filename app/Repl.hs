{-# LANGUAGE ScopedTypeVariables #-}

module Repl (repl) where

----------------------------------------------------------------------

import Control.Applicative
import Control.Exception
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Char qualified as Char
import Data.List (isPrefixOf)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Kriti
import Kriti.CustomFunctions (basicFuncMap)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Prettyprinter
import System.Console.Repline
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP

----------------------------------------------------------------------

repl :: IO ()
repl =
  flip evalStateT mempty $
    evalReplOpts $
      ReplOpts
        { banner = customBanner,
          command = command',
          options = options',
          prefix = Just ':',
          multilineCommand = Just "|",
          tabComplete = Prefix (wordCompleter defaultCompleter) prefixCompleters,
          initialiser = liftIO $ putStrLn "Kriti Lang, version 0.3.2: github.com/hasura/kriti-lang/ :? for help",
          finaliser = liftIO $ putStrLn "Goodbye!" >> pure Exit
        }

customBanner :: MultiLine -> HaskelineT (StateT (Map Text Aeson.Value) IO) String
customBanner SingleLine = pure "> "
customBanner MultiLine = pure "| "


options' :: [(String, String -> HaskelineT (StateT (Map Text Aeson.Value) IO) ())]
options' =
  [ ("?", \_ -> helpCommand),
    ("let", letCommand),
    ("dump", \_ -> dumpCommand)
  ]

----------------------------------------------------------------------

command' :: String -> HaskelineT (StateT (Map Text Aeson.Value) IO) ()
command' input = do
  ctx <- gets Map.toList
  case runKritiWith (Text.pack input) ctx basicFuncMap of
    Left err -> liftIO $ print $ pretty err
    Right json -> liftIO $ Char8.putStrLn $ encodePretty json

prefixCompleters :: MonadIO m => [(String, CompletionFunc m)]
prefixCompleters = [(":let", fileCompleter)]

defaultCompleter :: MonadState (Map Text Aeson.Value) m => WordCompleter m
defaultCompleter n = do
  ctx <- gets (fmap (Text.unpack . fst) . Map.toList)
  return $ filter (isPrefixOf n) ctx

----------------------------------------------------------------------

helpCommand :: HaskelineT (StateT (Map Text Aeson.Value) IO) ()
helpCommand =
  liftIO $
    print $
      vsep
        [ "Commands available from the prompt:",
          indent 2 $
            vsep
              [ ":?" <> indent 6 "display this help message",
                ":let" <> indent 4 "bind a json expression to a variable. You can also use filepaths and urls here.",
                ":dump" <> indent 3 "inspect all variables bound in this Kriti session"
              ]
        ]

----------------------------------------------------------------------

dumpCommand :: HaskelineT (StateT (Map Text Aeson.Value) IO) ()
dumpCommand = do
  ctx <- get
  void $
    liftIO $
      flip Map.traverseWithKey ctx $ \bndr json -> do
        Char8.putStrLn $ Char8.fromStrict (TE.encodeUtf8 bndr) <> " = " <> encodePretty json

----------------------------------------------------------------------

letCommand :: String -> HaskelineT (StateT (Map Text Aeson.Value) IO) ()
letCommand args = do
  case parseArgs args of
    Nothing -> liftIO $ print $ prettyParseError "Parser Error" "Unexpected Token" args
    Just (bndr, arg) -> do
      jsonM <- liftIO $ runMaybeT $ loadHttpRequestM arg <|> loadFileM arg <|> loadJSONM arg
      case jsonM of
        Just (Left err) -> liftIO $ print $ prettyParseError "Parser Error" err arg
        Just (Right json) -> modify $ Map.insert (Text.pack bndr) json
        Nothing -> liftIO $ print $ prettyParseError "Runtime Error" "Failed to parse :let command" arg

loadHttpRequestM :: String -> MaybeT IO (Either String Aeson.Value)
loadHttpRequestM uri =
  MaybeT $
    fmap Just (loadHttpRequest uri) `catch` \(_ :: HTTP.HttpException) -> pure Nothing

loadHttpRequest :: String -> IO (Either String Aeson.Value)
loadHttpRequest uri = do
  response <- liftIO $ do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    request <- HTTP.parseRequest uri
    HTTP.httpLbs request manager

  case HTTP.statusCode $ HTTP.responseStatus response of
    200 -> pure $ Aeson.eitherDecode @Aeson.Value $ HTTP.responseBody response
    _ -> pure $ Left $ show $ HTTP.statusMessage $ HTTP.responseStatus response

loadFileM :: String -> MaybeT IO (Either String Aeson.Value)
loadFileM path =
  MaybeT $
    fmap Just (loadFile path) `catch` \(_ :: SomeException) -> pure Nothing

loadFile :: String -> IO (Either String Aeson.Value)
loadFile path =
  let trim = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace
   in Aeson.eitherDecode @Aeson.Value <$> BL.readFile (trim path)

loadJSONM :: String -> MaybeT IO (Either String Aeson.Value)
loadJSONM bs =
  MaybeT $
    fmap Just (loadJSON bs) `catch` \(_ :: SomeException) -> pure Nothing

loadJSON :: String -> IO (Either String Aeson.Value)
loadJSON bs = pure $ Aeson.eitherDecode @Aeson.Value (Char8.pack bs)

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

prettyParseError :: String -> String -> String -> Doc ann
prettyParseError errorCode msg src =
  vsep
    [ pretty errorCode <> colon,
      indent 2 $ pretty msg,
      indent 4 $ "|",
      indent 4 $ "|" <+> pretty src,
      indent 4 $ "|"
    ]
