{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Exception.Safe (throwString)
import Control.Lens hiding ((<.>))
import Control.Monad.Except
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as JEP (encodePretty)
import Data.Aeson.Lens ()
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import Data.Either (isRight)
import Data.Foldable (for_)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TEL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Monoid
import Kriti
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
import Text.Read (readEither)
import Prettyprinter (Pretty) 

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  jsonParse
  jsonRoundTrip
  parserSpec
  evalSpec

--------------------------------------------------------------------------------
-- JSON Roundtripping test.

-- | Ensure that the grammar of Kriti is a superset of JSON with the
-- exception of '{{' characters. This is due to our templating
-- syntax. We should be able to parse any other JSON as a Kriti
-- expression.
jsonParse :: Spec
jsonParse = describe "JSON Parsing" $ do
  describe "Edge Cases" $ do
    it "can handle unicode escape sequences" $ do
      (P.parser $ UTF8.fromString "\"\\u001c\"") `shouldSatisfy` isRight
    it "can handle unicode keys" $ do
      (P.parser $ UTF8.fromString "\"σ\"") `shouldSatisfy` isRight
    it "can handle '{'" $ do
      (P.parser $ UTF8.fromString "\"{\"") `shouldSatisfy` isRight
    it "can handle '{{' when escaped properly" $ do
      (P.parser $ UTF8.fromString "\"\\{{\"") `shouldSatisfy` isRight
    it "can parse '\\{{' as '{{" $
      let res = evalBS "\"\\{{\""
       in case res of
            Left err -> expectationFailure $ show err
            Right _ -> res `shouldBe` Right (J.String "{{")
    it "can parse 'U+03C3' as 'σ'" $
      let res = evalBS "\"\\u03C3\""
       in case res of
            Left err -> expectationFailure $ show err
            Right _ -> res `shouldBe` Right (J.String "σ")
    it "can parse 'U+0041' as 'A'" $
      let res = evalBS "\"\\u0041\""
       in case res of
            Left err -> expectationFailure $ show err
            Right _ -> res `shouldBe` Right (J.String "A")
    it "can parse JSON as Kriti" $
      Q.property \(value :: J.Value) ->
        containsNoCurlies value Q.==> do
          let enc = BL.toStrict $ J.encode value
              res = first renderPretty $ P.parser enc
          case res of
            Left err -> expectationFailure $ T.unpack err
            Right _ -> pure ()

-- | The 'Arbitrary' instance for 'Value' will construct 'String' and
-- 'Object' key values with '{{'. Such values must be escaped in Kriti
-- due to our templating syntax. Therefore, we reject such generated
-- values quickcheck implication.
containsNoCurlies :: J.Value -> Bool
containsNoCurlies = getAll . foldMap (f checkStr) . universe
  where
    f :: (T.Text -> All) -> J.Value -> All
    f p = \case
      J.String str -> p str
      J.Object fields -> foldMap (p . fst) $ Compat.toList fields
      J.Array _ -> All True
      _ -> All True
    checkStr str = All $ not $ "{{" `T.isInfixOf` str

-- | Try to evaluate JSON as if it were a Kriti expression.
evalJson :: J.Value -> Either T.Text J.Value
evalJson value = do
  let enc = BL.toStrict $ J.encode value
  ast <- first renderPretty $ P.parser enc
  first renderPretty $ runEval enc ast []

evalBS :: BS.ByteString -> Either T.Text J.Value
evalBS input = do
  ast <- first renderPretty $ P.parser input
  first renderPretty $ runEval "" ast []

-- | Encode a JSON value as 'T.Text'.
encodeText :: J.Value -> T.Text
encodeText = TE.decodeUtf8 . BL.toStrict . J.encode

-- | Since Aeson Values do not contain Spans, we cannot check for
-- equality between ValueExt and JSON in a meaningful way. However, if
-- we parse /and evaluate/ JSON data then it should return the same
-- Value terms as Aeson.
jsonRoundTrip :: Spec
jsonRoundTrip = describe "JSON Roundtripping" $ do
  describe "QuickCheck" $ do
    it "matches Aeson for standard JSON values" $ do
      Q.property $ \(value :: J.Value) ->
        containsNoCurlies value Q.==> do
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
parseTemplateSuccess :: FilePath -> IO (BS.ByteString, Expr)
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
    Right valueExt -> throwString $ "Unexpected parsing success " <> T.unpack (renderPretty valueExt)

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

-- | Construct a 'Golden' test for any value with valid 'Read' and 'Pretty'
-- instances.
--
-- In this case, "valid" means that the value satisfies the roundtrip law where
-- @read . show === id@.
goldenReadShow ::
  (Read val, Pretty val) => FilePath -> String -> val -> Golden val
goldenReadShow dir name val = Golden {..}
  where
    output = val
    encodePretty = T.unpack . renderPretty
    writeToFile path actual =
      BS.writeFile path . TE.encodeUtf8 . renderPretty $ actual
    readFromFile path = do
      eVal <- readEither . TL.unpack <$> TLIO.readFile path
      either throwString pure eVal
    goldenFile = dir </> "golden" </> name <.> "txt"
    actualFile = Just $ dir </> "actual" </> name <.> "txt"
    failFirstTime = False

-- | Alias for 'goldenReadShow' specialized to 'ValueExt's.
goldenValueExt :: FilePath -> String -> Expr -> Golden Expr
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
    writeToFile path actual = BS.writeFile path . TE.encodeUtf8 . T.pack $ actual
    readFromFile path = T.unpack . TE.decodeUtf8 <$> BS.readFile path
    goldenFile = dir </> "golden" </> name <.> "txt"
    actualFile = Just $ dir </> "actual" </> name <.> "txt"
    failFirstTime = False

-- | Construct a 'Golden' test for any value with 'J.FromJSON' and 'J.ToJSON'
-- instances that are capable of "roundtripping" a response.
--
-- That is, something serialized with 'J.toJSON' can be read without error by
-- 'J.fromJSON'.
goldenAeson ::
  (J.FromJSON val, J.ToJSON val) => FilePath -> String -> val -> Golden val
goldenAeson dir name val = Golden {..}
  where
    output = val
    encodePretty = TL.unpack . TEL.decodeUtf8 . JEP.encodePretty
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

