{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Exception.Safe (throwString)
import Control.Monad (replicateM)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as JEP (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Foldable (for_)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Kriti.Aeson.Compat as Compat
import Kriti.Error
import Kriti.Eval
import qualified Kriti.Parser as P
import System.Directory (listDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Arbitrary.Generic as QAG
import Text.Pretty.Simple (pShowNoColor)
import Text.Read (readEither)

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  --lexerSpec
  parserSpec
  evalSpec

--------------------------------------------------------------------------------
-- Lexing tests.

-- | Lexer tests.
-- TODO: Round Trip tests don't make sense with spans unless we also
-- have a pretty printer. This test should be reintroduced and
-- rewritten once we have a working pretty printer.
lexerSpec :: Spec
lexerSpec = describe "Lexer" $
  describe "QuickCheck" $
    -- Note: This should be a pretty printer round trip test to account for spans
    it "lexes serialized tokens and yields those tokens modulo spans" $
      Q.property $ \tokens ->
        let serialized = T.intercalate " " $ fmap P.serialize tokens
            tokens' = P.lexer $ encodeUtf8 serialized
         in case tokens' of
              Left lexError -> expectationFailure (show $ render lexError)
              Right lexemes -> normalizeSpans lexemes `shouldBe` normalizeSpans (tokens :: [P.Token])

normalizeSpans :: [P.Token] -> [P.Token]
normalizeSpans = fmap (P.overLoc (P.setSpan (P.Span P.alexStartPos P.alexStartPos)))

--------------------------------------------------------------------------------
-- Parsing tests.

-- | Parser tests.
parserSpec :: Spec
parserSpec = describe "Parser" $ do
  parserGoldenSpec

-- TODO: Round Trip tests don't make sense with spans unless we also
-- have a pretty printer. This test should be reintroduced and
-- rewritten once we have a working pretty printer.
--
-- describe "QuickCheck" $
--   it "matches Aeson for standard JSON values" $
--     Q.property $ \value ->
--       let serialized = J.encode @J.Value value -- Serialized JSON via Aeson
--           tokens = P.parser $ BL.toStrict serialized -- Either _ ValueExt via kriti
--           --viaAeson = fromJust $ J.decode @P.ValueExt serialized
--        in case tokens of
--             Left err -> expectationFailure (show $ render err)
--             Right viaKriti ->
--               let serializedKriti = BL8.fromStrict $ encodeUtf8 $ P.serialize viaKriti
--               in case J.decode @J.Value serializedKriti of
--                 Nothing -> expectationFailure $ "Failed to roundtrip '" <> T.unpack (decodeUtf8 $ BL8.toStrict serialized) <> "'."
--                 Just _ -> pure () -- viaKriti `shouldBe` viaAeson

-- | 'Golden' parser tests for each of the files in the @examples@ subdirectory
-- found in the project directory hard-coded into this function.
parserGoldenSpec :: Spec
parserGoldenSpec = describe "Golden" $ do
  (dirSuc, pathsSuc) <- runIO $ fetchGoldenFiles "test/data/parser/success"
  (dirFail, pathsFail) <- runIO $ fetchGoldenFiles "test/data/parser/failure"

  describe "Success" $
    for_ pathsSuc $ \path -> do
      let name = dropExtension $ takeFileName path
      before (parseTemplateSuccess path) $
        it ("parses " <> name) $
          \valueExt -> goldenValueExt dirSuc name valueExt

  describe "Failure" $
    for_ pathsFail $ \path -> do
      let name = dropExtension $ takeFileName path
      before (parseTemplateFailure path) $
        it ("fails to parse " <> name) $
          \parseError -> goldenParseError dirFail name parseError

-- | Parse a template file that is expected to succeed; parse failures are
-- rendered as 'String's and thrown in 'IO'.
parseTemplateSuccess :: FilePath -> IO P.ValueExt
parseTemplateSuccess path = do
  tmpl <- BS.readFile $ path
  case P.parser tmpl of
    Left err -> throwString $ "Unexpected parsing failure " <> show err
    Right valueExt -> pure valueExt

-- | Parse a template file that is expected to fail.
parseTemplateFailure :: FilePath -> IO P.ParseError
parseTemplateFailure path = do
  tmpl <- BS.readFile $ path
  case P.parser tmpl of
    Left err -> pure err
    Right valueExt -> throwString $ "Unexpected parsing success " <> show valueExt

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
-- a 'source.json' file at the same path.
evalGoldenSpec :: Spec
evalGoldenSpec = describe "Golden" do
  (dir, paths) <- runIO $ fetchGoldenFiles "test/data/eval/success"
  source <- runIO $ do
    eSource <- J.eitherDecodeFileStrict (dir </> "source.json")
    either throwString pure eSource
  describe "Success" $
    for_ paths $ \path -> do
      let name = dropExtension $ takeFileName path
      before (evalSuccess source path) $
        it ("evaluates " <> name) $
          \json -> goldenAesonValue dir name json

-- | Parse an example file and evaluate it against the provided JSON source
-- file; any parsing or evaluation failures will be rendered as 'String's
-- and thrown in 'IO'.
evalSuccess :: J.Value -> FilePath -> IO J.Value
evalSuccess source path = do
  tmpl <- parseTemplateSuccess path
  either throwString pure $ either (Left . show) Right $ runEval tmpl [("$", source)]

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
goldenReadShow ::
  (Read val, Show val) => FilePath -> String -> val -> Golden val
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
goldenValueExt :: FilePath -> String -> P.ValueExt -> Golden P.ValueExt
goldenValueExt = goldenReadShow

-- | Construct a 'Golden' test for 'ParseError's rendered as 'String's.
--
-- Since 'ParseError' doesn't export a 'Show' instance that satisfies the
-- 'Read' <-> 'Show' roundtrip law, we must deal with its errors in terms of
-- the text it produces.
goldenParseError :: FilePath -> String -> P.ParseError -> Golden String
goldenParseError dir name parseError = Golden {..}
  where
    output = show $ render parseError
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
goldenAeson ::
  (J.FromJSON val, J.ToJSON val) => FilePath -> String -> val -> Golden val
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
alphabet = ['a' .. 'z'] ++ ['A' .. 'Z']

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

instance Q.Arbitrary P.AlexSourcePos where
  arbitrary = QAG.genericArbitrary
  shrink = QAG.genericShrink

instance Q.Arbitrary P.Symbol where
  arbitrary = QAG.genericArbitrary

instance Q.Arbitrary P.Token where
  arbitrary =
    QAG.genericArbitrary >>= \case
      P.TokNumLit _ i -> pure $ P.TokNumLit (T.pack $ show $ P.unLoc i) i
      P.TokIntLit _ i -> pure $ P.TokIntLit (T.pack $ show $ P.unLoc i) i
      P.EOF -> Q.arbitrary
      val -> pure val

instance (Q.Arbitrary a) => Q.Arbitrary (P.Loc a) where
  arbitrary = QAG.genericArbitrary
  shrink = QAG.genericShrink

instance Q.Arbitrary P.Span where
  arbitrary = QAG.genericArbitrary
  shrink = QAG.genericShrink

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
          object' = J.Object . Compat.fromList <$> Q.arbitrary

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
