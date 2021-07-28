module Main where

import Control.Monad
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text (Text)
import Test.Hspec

import qualified Data.Text as T
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Arbitrary.Generic as QAG

import GoBasic.Lexer
import GoBasic.Parser

main :: IO ()
main = hspec $ do
  checkLexer
  --checkParse
  --checkEval

checkLexer :: SpecWith ()
checkLexer = describe "Test Lexer" $
  it "lexing serialized tokens yields those tokens" $
    Q.property $ \tokens ->
      let serialized = T.intercalate " " $ fmap serialize tokens
      in (fmap teType <$> lexer) serialized `shouldBe` (tokens :: [Token])

alphabet :: String
alphabet = ['a'..'z'] ++ ['A'..'Z']

alphaNumerics :: String
alphaNumerics = alphabet ++ "0123456789"

whitespace :: Q.Gen Text
whitespace = do
  i <- Q.chooseInt (1, 10)
  spaces <- replicateM i $ Q.frequency [(10, pure (" " :: Text)), (1, pure "\n")]
  pure $ mconcat spaces

instance Q.Arbitrary Scientific where
  arbitrary = fromFloatDigits <$> Q.arbitrary @Float

instance Q.Arbitrary Text where
  arbitrary = do
    x <- Q.listOf1 (Q.elements alphabet)
    y <- Q.listOf1 (Q.elements alphaNumerics)
    pure $ T.pack $ x <> y

instance Q.Arbitrary Token where
  arbitrary = QAG.genericArbitrary

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
