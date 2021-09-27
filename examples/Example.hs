{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Text (Text)
import Kriti (runKriti)
import Network.HTTP.Client
import Text.RawString.QQ

exampleTemplate :: Text
exampleTemplate =
  [r|
{
   'id': {{$.id}},
   'name': {{$.name.english}},
   'description': {{$.description}},
   'profile': {
       'height': {{$.profile.height}},
       'weight': {{$.profile.weight}},
       'hp': {{$.base.HP}},
       'attack': {{$.base.Attack}},
       'defence': {{$.base.Defense}},
       'speed': {{$.base.Speed}}
   }
}
|]

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest "http://app.pokemon-api.xyz/pokemon/pikachu"
  response <- httpLbs request manager
  let parseResp = fromJust . J.decode $ responseBody response
  print $ runKriti exampleTemplate [("$", parseResp)]
