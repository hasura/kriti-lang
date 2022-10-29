{-# LANGUAGE QuasiQuotes #-}
module Main
  ( main,
  )
where

-------------------------------------------------------------------------------

import Test.Tasty.Bench (bench, defaultMain, nf)
import Prelude
import Kriti
import qualified Data.Aeson as J
import Data.Text (Text)
import Text.Shakespeare.Text (st)

-------------------------------------------------------------------------------

template1 :: Text
template1 = [st|
  "postgres://{{$.username}}:{{$.password}}@db.com:5432/{{$.database}}"
  |]
template2 :: Text
template2 = [st|
  {{ if $.request.session.x-hasura-role == "manager" }}
  "postgres://{{$.request.session.x-hasura-username}}:{{$.env['HASURA_POSTGRES_URL_PASSWORD']}}@db.com:5432/{{$.database}}"
  {{ else }}
  "postgres://{{$.username}}:{{$.password}}@db.com:5432/{{$.database}}"
  {{ end }}
  |]

template3 :: Text
template3  =[st|
  {{ if ($.request.query.operation_type == "query") || ($.request.query.operation_type == "subscription") }}
    "postgres://{{$.request.session.x-hasura-role}}:somepassword@db.com:5432/dbname"
  {{ else }}
      "{{$.env.HASURA_COMMON_POSTGRES_URL}}"
  {{ end }}
  |]

source :: J.Value
source = J.object [
  "username" J..= ("postgres" :: String),
  "password" J..= ("postgres" :: String),
  "database" J..= ("postgres" :: String),
  "env" J..= J.object [
    "HASURA_POSTGRES_URL_PASSWORD" J..= ("secretpassword" :: String),
    "HASURA_COMMON_POSTGRES_URL" J..= ("postgres://username:password@db.com:5432/database" :: String)
    ],
    "request" J..= J.object [
        "headers" J..= J.object [
            "device" J..= ("desktop" :: String),
            "cache" J..= False
        ],
        "session" J..= J.object [
            "x-hasura-username" J..= ("adminuser" :: String),
            "x-hasura-role" J..= ("manager" :: String)
        ],
        "query" J..= J.object [
            "operation_type" J..= ("query" :: String),
            "operation_name" J..= ("getUsersById" :: String)
        ]
    ]
  ]

main :: IO ()
main = do
  defaultMain [test1, test2, test3]
  where
    test1 = bench "template 1" $ nf (runKriti template1) [("$", source)]
    test2 = bench "template 2" $ nf (runKriti template2) [("$", source)]
    test3 = bench "template 3" $ nf (runKriti template3) [("$", source)]
