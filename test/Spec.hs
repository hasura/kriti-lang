{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Exception.Safe (throwString)
import Control.Monad.Except
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as JEP (encodePretty)
import Data.Bifunctor (first)
import Data.Either (isRight)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
-- [TODO: Reed M, 08/01/2022] Char8 is evil, and should be banished!
-- Using any of the functions from this module on unicode codepoints
-- above U+007F will cause issues.
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Foldable (for_)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
#if !MIN_VERSION_aeson(2,0,3)
import qualified Data.Vector as V
import qualified Kriti.Aeson.Compat as Compat
#endif
import Kriti
import Kriti.Error
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
  jsonParse
  jsonRoundTrip
  parserSpec
  evalSpec

--------------------------------------------------------------------------------
-- JSON Roundtripping test.

-- | Ensure that the grammar of Kriti is a superset of JSON.
-- In simpler terms, we should be able to parse any JSON as
-- a Kriti expression.
jsonParse :: Spec  
jsonParse = describe "JSON Parsing" $ do
    describe "Edge Cases" $ do
      it "can handle unicode escape sequences" $ do
        (parser $ UTF8.fromString "\"\\u001c\"") `shouldSatisfy` isRight
      it "can handle unicode keys" $ do
        (parser $ UTF8.fromString "\"σ\"") `shouldSatisfy` isRight
    describe "QuickCheck" $ do
      it "can parse JSON as Kriti" $
        Q.property \(value :: J.Value) ->
          let enc = BL.toStrict $ J.encode value
              res = first renderPretty $ parser enc
          in case res of
            Left err -> expectationFailure $ T.unpack err
            Right _ -> pure ()

-- | Try to evaluate JSON as if it were a Kriti expression.
evalJson :: J.Value -> Either T.Text J.Value
evalJson value = do
    let enc = BL.toStrict $ J.encode value
    ast <- first renderPretty $ parser enc
    first renderPretty $ runEval enc ast []

-- | Encode a JSON value as 'T.Text'.
encodeText :: J.Value -> T.Text
encodeText = TE.decodeUtf8 . BL.toStrict . J.encode

-- | Since Aeson Values do not contain Spans, we cannot check for
-- equality between ValueExt and JSON in a meaningful way. However, if
-- we parse /and evaluate/ JSON data then it should return the same
-- Value terms as Aeson.
jsonRoundTrip :: Spec
jsonRoundTrip = describe "JSON Roundtripping" $ do
  describe "Edge Cases" $ do
    it "can roundtrip unicode" $ do
        let result = evalJson $ J.Object $ Compat.fromList [("σ", J.Null)]
        result `shouldSatisfy` isRight
  describe "QuickCheck" $ do
    it "matches Aeson for standard JSON values" $ do
      Q.property $ \(value :: J.Value) -> do
        result <- runExceptT $ do
            json <- hoistEither $ evalJson value
            if json == value
            then pure ()
            else throwError $ "Failed to roundtrip '" <> encodeText value <> "', got '" <> encodeText json <> "'."
        case result of
          Left err -> expectationFailure $ T.unpack err
          Right _ -> pure ()

--------------------------------------------------------------------------------
-- Parsing tests.

-- | Parser tests.
parserSpec :: Spec
parserSpec = describe "Parser" $ do
  parserGoldenSpec

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
          \(_, valueExt) -> goldenValueExt dirSuc name valueExt

  describe "Failure" $
    for_ pathsFail $ \path -> do
      let name = dropExtension $ takeFileName path
      before (parseTemplateFailure path) $
        it ("fails to parse " <> name) $
          \parseError -> goldenParseError dirFail name parseError

-- | Parse a template file that is expected to succeed; parse failures are
-- rendered as 'String's and thrown in 'IO'.
parseTemplateSuccess :: FilePath -> IO (BS.ByteString, ValueExt)
parseTemplateSuccess path = do
  tmpl <- BS.readFile $ path
  case P.parser tmpl of
    Left err -> throwString $ "Unexpected parsing failure " <> show err
    Right valueExt -> pure (tmpl, valueExt)

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
  (src, tmpl) <- parseTemplateSuccess path
  either throwString pure $ either (Left . show) Right $ runEval src tmpl [("$", source)]

--------------------------------------------------------------------------------
-- Golden test construction functions.

-- | Construct a 'Golden' test for any value with valid 'Read' and 'Show'
-- instances.
--
-- In this case, "valid" means that the value satisfies the roundtrip law where
-- @read . show === id@.
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
goldenValueExt :: FilePath -> String -> ValueExt -> Golden ValueExt
goldenValueExt = goldenReadShow

-- | Construct a 'Golden' test for 'ParseError's rendered as 'String's.
--
-- Since 'ParseError' doesn't export a 'Show' instance that satisfies the
-- 'Read' <-> 'Show' roundtrip law, we must deal with its errors in terms of
-- the text it produces.
goldenParseError :: FilePath -> String -> P.ParseError -> Golden String
goldenParseError dir name parseError = Golden {..}
  where
    output = show $ serialize parseError
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

instance Q.Arbitrary T.Text where
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

#if !MIN_VERSION_aeson(2,0,3)
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
#endif

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

hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . pure

