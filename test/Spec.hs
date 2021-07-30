{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Main where

import Control.Exception.Safe (throwString)
import Control.Monad (replicateM)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (listDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
import Text.Parsec (ParseError)
import Text.Pretty.Simple (pShowNoColor)
import Text.Read (readEither)

import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as JEP (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Arbitrary.Generic as QAG

import GoBasic.Lexer
import GoBasic.Parser
import GoBasic.Eval

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  lexerSpec
  parserSpec
  evalSpec

--------------------------------------------------------------------------------
-- Lexing tests.

-- | Lexer tests.
lexerSpec :: Spec
lexerSpec = describe "Lexer" $
  describe "QuickCheck" $
    it "lexing serialized tokens yields those tokens" $
      Q.property $ \tokens ->
        let serialized = T.intercalate " " $ fmap serialize tokens
        in (fmap teType <$> lexer) serialized `shouldBe` (tokens :: [Token])

--------------------------------------------------------------------------------
-- Parsing tests.

-- | Parser tests.
parserSpec :: Spec
parserSpec = describe "Parser" $ do
  parserGoldenSpec
  describe "QuickCheck" $
    it "matches Aeson for standard JSON values" $
      Q.property $ \value ->
        let serialized = J.encode @J.Value value
            tokens = lexer $ decodeUtf8 $ BL.toStrict serialized
            viaAeson = fromJust $ J.decode @ValueExt serialized
        in parse tokens `shouldSatisfy` succeeds viaAeson

-- | 'Golden' parser tests for each of the files in the @examples@ subdirectory
-- found in the project directory hard-coded into this function.
parserGoldenSpec :: Spec
parserGoldenSpec = do
  (dir, paths) <- runIO $ fetchGoldenFiles "test/data/parser"
  describe "Golden" $ do
    describe "Success" $ for_ paths $ \path -> do
      let name = takeFileName path
      before (parseTemplateSuccess path) $ it ("parses " <> name) $
        \valueExt -> goldenValueExt dir name valueExt

  describe "Failure" $ pure ()

-- | Parse a template file that is expected to succeed; parse failures are
-- rendered as 'String's and thrown in 'IO'.
parseTemplateSuccess :: FilePath -> IO ValueExt
parseTemplateSuccess path = do
  tmpl <- fmap decodeUtf8 . BS.readFile $ path
  case parse $ lexer tmpl of
    Left err -> throwString $ "Unexpected parsing failure " <> show err
    Right valueExt -> pure valueExt

--------------------------------------------------------------------------------
-- Evaluation tests.

-- | Evaluation tests.
evalSpec :: Spec
evalSpec = describe "Eval" $ do
  evalGoldenSpec

-- | 'Golden' evaluation tests for each of the files in the @examples@
-- subdirectory found in the project directory hard-coded into this function.
--
-- NOTE: In addition to the @examples@ directory, this function also depends on
--
evalGoldenSpec :: Spec
evalGoldenSpec = do
  (dir, paths) <- runIO $ fetchGoldenFiles "test/data/eval"
  source <- runIO $ do
    eSource <- J.eitherDecodeFileStrict (dir </> "source.json")
    either throwString pure eSource
  describe "Golden" $ do
    describe "Success" $ for_ paths $ \path -> do
      let name = takeFileName path
      before (evalSuccess source path) $ it ("evaluates " <> name) $
        \json -> goldenAesonValue dir name json

-- | Parse an example file and evaluate it against the provided JSON source
-- file; any parsing or evaluation failures will be rendered as 'String's
-- and thrown in 'IO'.
evalSuccess :: J.Value -> FilePath -> IO J.Value
evalSuccess source path = do
  tmpl <- parseTemplateSuccess path
  either throwString pure $ runEval tmpl source

--------------------------------------------------------------------------------
-- Golden test construction functions.

-- | Construct a 'Golden' test for any value with valid 'Read' and 'Show'
-- instances.
--
-- In this case, "valid" means that the value satisfies the roundtrip law where
-- @read . show === id@.
--
-- NOTE: This function uses the 'goldenDir' function defined within this module
-- to read/write files from/to a shared directory within the project.
goldenReadShow
  :: (Read val, Show val) => FilePath -> String -> val -> Golden val
goldenReadShow dir name val = Golden {..}
  where
    output = val
    encodePretty = TL.unpack . pShowNoColor
    writeToFile path actual =
      BS.writeFile path . BS8.pack . TL.unpack . pShowNoColor $ actual
    readFromFile path = do
      eVal <- readEither . BS8.unpack <$> BS.readFile path
      either throwString pure eVal
    goldenFile = dir </> "golden" </> name <.> "txt"
    actualFile = Just $ dir </> "actual" </> name <.> "txt"
    failFirstTime = False

-- | Alias for 'goldenReadShow' specialized to 'ValueExt's.
goldenValueExt :: FilePath -> String -> ValueExt -> Golden ValueExt
goldenValueExt = goldenReadShow

-- | Construct a 'Golden' test for 'ParseError's rendered as 'String's.
--
-- Since 'ParseError' doesn't export a 'Show' instance that satisfies the
-- 'Read' <-> 'Show' roundtrip law, we must deal with its errors in terms of
-- the text it produces.
goldenParseError :: FilePath -> String -> ParseError -> Golden String
goldenParseError dir name parseError = Golden{..}
  where
    output = show parseError
    encodePretty = id
    writeToFile path actual = BS.writeFile path . BS8.pack $ actual
    readFromFile path = BS8.unpack <$> BS.readFile path
    goldenFile = dir </> "golden" </> name <.> "txt"
    actualFile = Just $ dir </> "actual" </> name <.> "txt"
    failFirstTime = False

-- | Construct a 'Golden' test for any value with 'J.FromJSON' and 'J.ToJSON'
-- instances that are capable of "roundtripping" a response.
--
-- That is, something serialized with 'J.toJSON' can be read without error by
-- 'J.fromJSON'.
--
-- NOTE: If
goldenAeson
  :: (J.FromJSON val, J.ToJSON val) => FilePath -> String -> val -> Golden val
goldenAeson dir name val = Golden {..}
  where
    output = val
    encodePretty = BL8.unpack . JEP.encodePretty
    writeToFile path actual =
      BL.writeFile path . JEP.encodePretty $ actual
    readFromFile path = do
      eValue <- J.eitherDecodeFileStrict path
      either throwString pure eValue
    goldenFile = dir </> "golden" </> name <.> "json"
    actualFile = Just $ dir </> "actual" </> name <.> "json"
    failFirstTime = False

-- | Alias for 'goldenAeson' specialized to 'J.Value's.
goldenAesonValue :: FilePath -> String -> J.Value -> Golden J.Value
goldenAesonValue = goldenAeson

--------------------------------------------------------------------------------
-- QuickCheck helpers and orphan instances.

alphabet :: String
alphabet = ['a'..'z'] ++ ['A'..'Z']

alphaNumerics :: String
alphaNumerics = alphabet ++ "0123456789"

whitespace :: Q.Gen Text
whitespace = do
  i <- Q.chooseInt (1, 10)
  spaces <- replicateM i $ Q.frequency [(10, pure (" " :: Text)), (1, pure "\n")]
  pure $ mconcat spaces

instance Q.Arbitrary Text where
  arbitrary = do
    x <- Q.listOf1 (Q.elements alphabet)
    y <- Q.listOf1 (Q.elements alphaNumerics)
    pure $ T.pack $ x <> y

instance Q.Arbitrary Scientific where
  arbitrary = ((fromRational . toRational) :: Int -> Scientific) <$> Q.arbitrary

instance Q.Arbitrary Token where
  arbitrary = QAG.genericArbitrary

instance Q.Arbitrary J.Value where
  arbitrary = Q.sized sizedArbitraryValue
    where
      sizedArbitraryValue n
        | n <= 0 = Q.oneof [pure J.Null, boolean', number', string']
        | otherwise = Q.resize n' $ Q.oneof [pure J.Null, boolean', number', string', array', object']
        where
          n' = n `div` 2
          boolean' = J.Bool <$> Q.arbitrary
          number' = J.Number <$> Q.arbitrary
          string' = J.String <$> Q.arbitrary
          array' = J.Array . V.fromList <$> Q.arbitrary
          object' = J.Object . M.fromList <$> Q.arbitrary

--------------------------------------------------------------------------------
-- General test helpers.

-- | Fetches example files for golden tests from the @examples@ subdirectory
-- at the given 'FilePath'.
--
-- We assume that the directory at the given 'FilePath' has the following
-- structure:
--  * /actual
--  * /examples
--  * /golden
fetchGoldenFiles :: FilePath -> IO (FilePath, [FilePath])
fetchGoldenFiles dir = do
  let exampleDir = dir </> "examples"
  examples <- listDirectory exampleDir
  pure (dir, map (exampleDir </>) examples)

succeeds :: Eq a => a -> Either e a -> Bool
succeeds s (Right s') = s == s'
succeeds _ _ = False

fails :: Either e a -> Bool
fails (Right _) = False
fails _ = True
