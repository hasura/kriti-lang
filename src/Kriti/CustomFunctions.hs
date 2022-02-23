module Kriti.CustomFunctions
  ( basicFuncMap,
    emptyF,
    lengthF,
    inverseF,
    headF,
    tailF,
    toCaseFoldF,
    toLowerF,
    toUpperF,
    toTitleF,
  )
where

import qualified Data.Aeson as J
import qualified Data.HashMap.Internal as Map
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Kriti.Error (CustomFunctionError (..))

type KritiFunc = J.Value -> Either CustomFunctionError J.Value

-- | Basic custom function for Kriti.
basicFuncMap :: Map.HashMap T.Text KritiFunc
basicFuncMap =
  Map.fromList
    [ ("empty", emptyF),
      ("size", lengthF),
      ("inverse", inverseF),
      ("head", headF),
      ("tail", tailF),
      ("toCaseFold", toCaseFoldF),
      ("toLower", toLowerF),
      ("toUpper", toUpperF),
      ("toTitle", toTitleF)
    ]

emptyF :: KritiFunc
emptyF inp = case inp of
  J.Object km -> Right . J.Bool $ null km
  J.Array vec -> Right . J.Bool $ null vec
  J.String txt -> Right . J.Bool $ T.strip txt == ""
  J.Number sci -> Right . J.Bool $ sci == 0
  J.Bool _ -> Left . CustomFunctionError $ "Cannot define emptiness for a boolean"
  J.Null -> Right $ J.Bool True

lengthF :: KritiFunc
lengthF inp = Right . J.Number $ case inp of
  J.Object km -> (`S.scientific` 0) . toInteger $ length km
  J.Array vec -> (`S.scientific` 0) . toInteger $ length vec
  J.String txt -> (`S.scientific` 0) . toInteger $ T.length txt
  J.Number sci -> sci
  J.Bool b -> (`S.scientific` 0) . toInteger $ fromEnum b
  J.Null -> 0

inverseF :: KritiFunc
inverseF inp = case inp of
  J.Object km -> Right . J.Object $ km
  J.Array vec -> Right . J.Array $ V.reverse vec
  J.String txt -> Right . J.String $ T.reverse txt
  J.Number sci -> Right . J.Number $ 1 / sci
  J.Bool b -> Right . J.Bool $ not b
  J.Null -> Right J.Null

headF :: KritiFunc
headF inp = case inp of
  J.Array vec -> case V.uncons vec of
    Nothing -> Left . CustomFunctionError $ "Empty array"
    Just (x, _) -> Right x
  J.String txt -> case T.uncons txt of
    Nothing -> Left . CustomFunctionError $ "Empty string"
    Just (x, _) -> Right . J.String . T.singleton $ x
  _ -> Left . CustomFunctionError $ "Expected an array or string"

tailF :: KritiFunc
tailF inp = case inp of
  J.Array vec -> Right . J.Array $ V.tail vec
  J.String txt -> Right . J.String $ T.tail txt
  _ -> Left . CustomFunctionError $ "Expected an array or string"

toCaseFoldF :: KritiFunc
toCaseFoldF inp = case inp of
  J.String txt -> Right . J.String $ T.toCaseFold txt
  _ -> Left . CustomFunctionError $ "Expected string"

toLowerF :: KritiFunc
toLowerF inp = case inp of
  J.String txt -> Right . J.String $ T.toLower txt
  _ -> Left . CustomFunctionError $ "Expected string"

toUpperF :: KritiFunc
toUpperF inp = case inp of
  J.String txt -> Right . J.String $ T.toUpper txt
  _ -> Left . CustomFunctionError $ "Expected string"

toTitleF :: KritiFunc
toTitleF inp = case inp of
  J.String txt -> Right . J.String $ T.toTitle txt
  _ -> Left . CustomFunctionError $ "Expected string"
