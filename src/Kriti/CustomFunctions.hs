{-# LANGUAGE ScopedTypeVariables #-}
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
    objectToArray,
    arrayToObject,
    parserToFunc,
    concatArrays
  )
where

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
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
      ("toTitle", toTitleF),
      ("arrayToObject", arrayToObject),
      ("objectToArray", objectToArray),
      ("removeNulls", removeNulls),
      ("concatArrays", concatArrays)
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

-- | Convert an Object like `{ a:b, c:d ... }` to an Array like `[ [a,b], [c,d] ... ]`.
objectToArray :: KritiFunc
objectToArray = parserToFunc $ J.withObject "Object" \o -> do
  let km :: KM.KeyMap J.Value = o
      l :: [(J.Key, J.Value)] = KM.toList km
    in pure . J.Array $ V.fromList $ map (\(a, b) -> J.Array $ V.fromList [J.String $ K.toText a,b]) l

-- | Convert an Array like `[ [a,b], [c,d] ... ]` to an Object like `{ a:b, c:d ... }`.
arrayToObject :: KritiFunc
arrayToObject = parserToFunc $ J.withArray "Nested Arrays" \vec -> do
  J.Object . KM.fromList . V.toList <$> traverse mkPair vec

  where
  shapeErr = "Expected an array of shape [ [k1,v1], [k2,v2] ... ] - With String keys."

  mkPair = J.withArray "Array of Pair" \vec -> do
    case V.toList vec of
      [k,v] -> flip (J.withText "String") k \t -> pure (K.fromText t, v)
      _ -> fail shapeErr

removeNulls :: KritiFunc
removeNulls = parserToFunc $ J.withArray "Array" \a -> do
  pure $ J.Array $ V.filter notNull a
  where
  notNull J.Null = False
  notNull _ = True

concatArrays :: KritiFunc
concatArrays = parserToFunc $ J.withArray "Array" \as -> do
  as' <- traverse (J.withArray "Nested Array" pure) (V.toList as)
  pure $ J.Array $ V.concat as'

-- | Converts an Aeson Parser into a KritiFunc
--   The value-to-parser argument's type matches the `parseJson` type from FromJSON
--   allowing Aeson's instances to be reused as KritiFuncs if they return values.
parserToFunc :: (J.Value -> J.Parser J.Value) -> KritiFunc
parserToFunc p v = case J.parse p v of
  J.Error e -> Left (CustomFunctionError (T.pack e))
  J.Success r -> Right r