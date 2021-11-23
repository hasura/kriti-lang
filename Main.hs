module Main where

-- import Control.Exception

import Control.Monad (void)
import Data.Aeson (decode, encode)
import Data.Bifoldable (bifoldMap)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (fromJust)
import Data.Text
import Kriti (runKriti)
import Options.Applicative
import System.IO (IOMode (ReadMode), openFile)

data KritiOptions = KritiOptions
  { _koJSONFile :: FilePath,
    _koTemplateFile :: FilePath,
    _koBindingSymbol :: Text
  }
  deriving (Show, Eq)

-- TODO: we should add support for STDIN too
kritiOpts :: Parser KritiOptions
kritiOpts =
  KritiOptions
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

main :: IO ()
main = runKritiInteractive =<< execParser opts
  where
    opts =
      info (kritiOpts <**> helper) $
        fullDesc
          <> progDesc "transform JSON using the Kriti language"
          <> header "kriti - a minimal JSON templating language based on Go's template language."

runKritiInteractive :: KritiOptions -> IO ()
runKritiInteractive (KritiOptions jsonFile templateFile rootSymbol) = do
  void $ checkFilePath jsonFile
  void $ checkFilePath templateFile

  json <- LBS.readFile jsonFile
  template <- readFile templateFile
  LBS8.putStrLn $ bifoldMap encode encode $ runKriti (pack template) [(rootSymbol, fromJust . decode $ json)]
  where
    checkFilePath = flip openFile ReadMode
