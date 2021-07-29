module Main where

import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Test.Hspec
import Test.Hspec.Golden
import Text.Parsec (ParseError)
import Text.Parsec.Error (errorMessages)
import Text.RawString.QQ

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Arbitrary.Generic as QAG

import GoBasic.Lexer
import GoBasic.Parser

main :: IO ()
main = hspec $ do
  checkLexer
  checkParse
  --checkEval

checkLexer :: SpecWith ()
checkLexer = describe "Test Lexer" $
  describe "QuickCheck Lexer Tests" $
  it "lexing serialized tokens yields those tokens" $
    Q.property $ \tokens ->
      let serialized = T.intercalate " " $ fmap serialize tokens
      in (fmap teType <$> lexer) serialized `shouldBe` (tokens :: [Token])

checkParse :: SpecWith ()
checkParse = describe "Test Parser" $ do
  describe "Explicit Parser Tests" $
    mapM_ (uncurry specParseYields) parseCases
  spec
  describe "QuickCheck Parser Tests" $
    it "Parser matches Aeson for standard JSON values" $
      Q.property $ \value ->
        let serialized = J.encode @J.Value value
            tokens = lexer $ decodeUtf8 $ BL.toStrict serialized
            viaAeson = fromJust $ J.decode @ValueExt serialized
        in parse tokens `shouldSatisfy` succeeds viaAeson

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

--genTokenExt :: Q.Gen [TokenExt]
--genTokenExt = do
--    numTokens <- Q.chooseInt (1, 100)
--    tokens <- sequence $ replicate numTokens (Q.arbitrary :: Q.Gen Token)
--    spaces <- sequence $ replicate numTokens whitespace
--    let f (toks, pos) (tok, spc) =
--          let (_, pos') = advance spc pos (prettyPrint tok)
--              tokenExt = TokenExt tok pos
--          in pure (toks <> [tokenExt], pos')
--
--    fmap fst $ foldM f ([], initialPos "src") $ zip tokens spaces

succeeds :: Eq a => a -> Either e a -> Bool
succeeds s (Right s') = s == s'
succeeds _ _ = False

fails :: Either e a -> Bool
fails (Right _) = False
fails _ = True

specLexerYields :: Text -> [TokenExt] -> SpecWith ()
specLexerYields s expected =
    it ("lex " ++ show s ++ " as " ++ show expected) $
        lexer s `shouldSatisfy` (== expected)

specParseYields :: [TokenExt] -> ValueExt -> SpecWith ()
specParseYields s expected =
    it ("parses " ++ show s ++ " as " ++ show expected) $
        parse s `shouldSatisfy` succeeds expected

specParseFails :: [TokenExt] -> SpecWith ()
specParseFails s =
    it ("fails to parse " ++ show s) $
        parse s `shouldSatisfy` fails

parseCases :: [([TokenExt], ValueExt)]
parseCases = fmap (first lexer) $
  [ ("null", Null)
  , ("true", Boolean True)
  , ("false", Boolean False)
  , ("1", Number 1)
  , ("1.5", Number 1.5)
  , ("\"hello\"", String "hello")
  , ("\"hello\"", String "hello")
  , ("\"hello123\"", String "hello123")
  , ("[1, null, true]", Array $ V.fromList [Number 1, Null, Boolean True])
  , ("{\"foo\": 1}", Object (M.singleton "foo" (Number 1)))
  , ("{\"foo\": [1]}", Object (M.singleton "foo" (Array $ pure $ Number 1)))
  , ("(| $.foo.bar[1] |)", Path [Obj "$", Obj "foo", Obj "bar", Arr 1])
  , ("(| x.foo.bar[1] |)", Path [Obj "x", Obj "foo", Obj "bar", Arr 1])
  , ("(| if $.foo |) 1 (| else |) null (|end |)", Iff (Path [Obj "$", Obj "foo"]) (Number 1) Null)
  -- , (fullTemplateEx, fullTemplateExAST)
  ]


fullTemplateEx :: Text
fullTemplateEx = [r|{
  "author": {
    "name": (|$.event.name|),
    "age": (|$.event.age|),
    "articles": [
(| range _, x := $.event.author.articles |)
      {
        "id": (|x.id|),
        "title": (|x.title|)
      }
(| end |)
    ]
  }
}
|]

fullTemplateExAST :: ValueExt
fullTemplateExAST =
  Object $ M.fromList
    [ ("author"
      , Object $ M.fromList
        [ ("name", Path [Obj "$", Obj "event", Obj "name"])
        , ("age", Path [Obj "$", Obj "event", Obj "age"])
        , ("articles"
          , Range Nothing "x" [Obj "$", Obj "event", Obj "author", Obj "articles"]
              (Object $ M.fromList
                 [ ("id", Path [Obj "x", Obj "id"])
                 , ("title", Path [Obj "x", Obj "title"])
                 ]
              )
          )
        ]
      )
    ]


goldenParseResult :: String -> Either String ValueExt -> Golden (Either String ValueExt)
goldenParseResult name parseResult =
  Golden
    { output = parseResult
    , encodePretty = show
    , writeToFile = \path val -> BL.writeFile path (BLU.fromString $ show val)
    , readFromFile = \path -> read @(Either String ValueExt) . BLU.toString <$> BL.readFile path
    , goldenFile = "test/data/" <> name
    , actualFile = Nothing
    , failFirstTime = False
    }

spec :: Spec
spec =
--(BL.readFile "test/data/example1.json")
  describe "trial Golden test" $ do
   before (TIO.readFile "test/data/example1.json")  $
     it "trying to run a golden test" \file -> do
      let res = either (Left . show) Right $ parse $ lexer file
      goldenParseResult "example1" res
