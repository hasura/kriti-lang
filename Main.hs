module Main where

import Control.Monad.Except
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Kriti (renderPretty, runKritiBSWith)
import Kriti.CustomFunctions (basicFuncMap)
import Options.Applicative
import Prettyprinter
import System.IO (IOMode (ReadMode), openFile)

data KritiOptions = KritiOptions
  { _koJSONFile :: FilePath,
    _koTemplateFile :: FilePath,
    _koBindingSymbol :: T.Text
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
main = do
  let parserOptions =
        info (kritiOpts <**> helper) $
          fullDesc
            <> progDesc "transform JSON using the Kriti language"
            <> header "kriti - a minimal JSON templating language based on Go's template language."
  kritiOptions <- execParser parserOptions
  result <- runExceptT $ runKriti kritiOptions
  either (print . pretty) (print . J.encode) result

runKriti :: KritiOptions -> ExceptT T.Text IO J.Value
runKriti (KritiOptions jsonFile templateFile rootSymbol) = do
  let checkFilePath = flip openFile ReadMode
  void $ liftIO $ checkFilePath jsonFile
  void $ liftIO $ checkFilePath templateFile

  json <- ExceptT $ first T.pack . J.eitherDecode <$> LBS.readFile jsonFile
  template <- liftIO $ B.readFile templateFile
  ExceptT $ pure $ first renderPretty $ runKritiBSWith template [(rootSymbol, json)] basicFuncMap
