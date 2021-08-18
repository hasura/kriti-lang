{-# LANGUAGE OverloadedStrings #-}
module Main where

import              Control.Monad.IO.Class
import qualified    Data.Aeson              as J
import qualified    Data.ByteString.Lazy    as BL
import              Data.Maybe
import              GoBasic                 (runGoBasic)
import              Network.HTTP.Req

getPokemon :: MonadHttp m => m BsResponse
getPokemon = req GET (https "app.pokemon-api.xyz" /: "pokemon" /: "pikachu") NoReqBody bsResponse mempty

myTemp = 
    "{\
    \   'id': {{$.id}},\
    \   'name': {{$.name.english}},\
    \   'description': {{$.description}},\
    \   'profile': {\
    \       'height': {{$.profile.height}},\
    \       'weight': {{$.profile.weight}},\
    \       'hp': {{$.base.HP}},\
    \       'attack': {{$.base.Attack}},\
    \       'defence': {{$.base.Defense}},\
    \       'speed': {{$.base.Speed}}\
    \   }\
    \}"

-- TODO: parse the following too
--    \       'spAttack': {{$.base.Sp. Attack}},\
--    \       'spDefence': {{$.base.Sp. Defense}},\

main :: IO ()
main = runReq defaultHttpConfig $ do
    pokeResp <- getPokemon
    let
        parseResp = fromJust . J.decode . BL.fromStrict . responseBody $ pokeResp
    liftIO $ print $ runGoBasic myTemp [("$", parseResp)]