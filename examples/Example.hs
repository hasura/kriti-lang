{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.Aeson as J
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as K
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Internal as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Kriti (renderPretty, runKritiWith)
import Kriti.Error (CustomFunctionError (..))
import Prettyprinter
import Text.RawString.QQ

exampleTemplate :: Text
exampleTemplate =
  [r|
{{ range i, x := $.results }}
  {
    "id" : {{i}},
    "fullName" : {{concatName x.name}},
    "profile" : {
      "gender" : {{getG x.gender}},
      "emailID": {{x.email}},
      "isSuperUser" : {{isAdmin x.login.username}}
    }
  }
{{ end }}
|]

main :: IO ()
main = do
  parseResp <- fromJust . J.decode <$> LBS.readFile "examples/source.json"
  let getNameUnsafe :: J.Object -> Text
      getNameUnsafe obj = T.intercalate " " $ map (`lk` obj) ["title", "first", "last"]
        where
          lk t ob = case K.lookup (K.fromText t) ob of
            Just (J.String txt) -> txt
            _ -> error . T.unpack $ t <> " not found"

      mkName :: J.Value -> Either CustomFunctionError J.Value
      mkName inp = case inp of
        J.Object km -> Right . J.String $ getNameUnsafe km
        _ -> Left $ CustomFunctionError "expected an object in name"

      getGender :: J.Value -> Either CustomFunctionError J.Value
      getGender inp = case inp of
        J.String txt -> Right . J.String . T.singleton . T.head . T.toUpper $ txt
        _ -> Left $ CustomFunctionError "expected gender to be a string"

      isAdmin :: J.Value -> Either CustomFunctionError J.Value
      isAdmin inp = case inp of
        J.String txt -> Right . J.Bool $ txt == "admin"
        _ -> Left $ CustomFunctionError "expected usename to be a string"

      functionMap = Map.fromList [("concatName", mkName), ("getG", getGender), ("isAdmin", isAdmin)]

  either (print . pretty) (print . J.encode) $
    first renderPretty $
      runKritiWith exampleTemplate [("$", parseResp)] functionMap
