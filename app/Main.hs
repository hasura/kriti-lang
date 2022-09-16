module Main where

----------------------------------------------------------------------

import Control.Monad.Except
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import Kriti (KritiError (..), runKritiBSWith)
import Kriti.CustomFunctions (basicFuncMap)
import Options.Applicative
import Prettyprinter
import Repl
import System.IO (IOMode (ReadMode), openFile)

----------------------------------------------------------------------

main :: IO ()
main = do
  let parserOptions =
        info (kritiCommand <**> helper) $
          fullDesc
            <> progDesc "transform JSON using the Kriti language"
            <> header "kriti - a minimal JSON templating language based on Go's template language."
  execParser parserOptions >>= \case
    Exec kritiOptions -> do
      result <- runKriti kritiOptions
      either (print . pretty) (C8.putStrLn . J.encode) result
    Repl -> repl

runKriti :: ExecOptions -> IO (Either KritiError J.Value)
runKriti (ExecOptions jsonFile templateFile rootSymbol) = do
  let checkFilePath = flip openFile ReadMode
  void $ liftIO $ checkFilePath jsonFile
  void $ liftIO $ checkFilePath templateFile

  template <- liftIO $ B.readFile templateFile
  first T.pack . J.eitherDecode <$> LBS.readFile jsonFile >>= \case
    Left err -> pure $ Left $ JsonDecodeError err
    Right json -> pure $ runKritiBSWith template [(rootSymbol, json)] basicFuncMap

----------------------------------------------------------------------
-- Arg Parser

data CommandOptions = Exec ExecOptions | Repl

data ExecOptions = ExecOptions
  { _koJSONFile :: FilePath,
    _koTemplateFile :: FilePath,
    _koBindingSymbol :: T.Text
  }
  deriving (Show, Eq)

kritiCommand :: Parser CommandOptions
kritiCommand =
  subparser
    ( command
        "exec"
        ( info
            (helper <*> fmap Exec execCommand)
            (progDesc "Apply a kriti transformation to a file")
        )
        <> command
          "repl"
          ( info
              (helper <*> pure Repl)
              (progDesc "Run the Kriti Repl")
          )
    )

-- TODO: we should add support for STDIN. The user would load a
-- template with argv and then pipe in a json bytestring.
execCommand :: Parser ExecOptions
execCommand =
  ExecOptions
    <$> strOption
      ( long "json" <> short 'j' <> metavar "JSON_FILE"
          <> help "The JSON file to read"
      )
      <*> strOption
        ( long "template" <> short 't' <> metavar "TEMPLATE_FILE"
            <> help "The template file to use"
        )
      <*> strOption
        ( long "bind" <> short 'b' <> metavar "BINDING_SYMBOL"
            <> showDefault
            <> value "$"
            <> help "The symbol that's used to represent a JSON binding within the template"
        )
