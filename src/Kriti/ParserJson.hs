{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


-- | This module allows for a pure-json representation of Kriti templates.
--   The downside is that you lose convenient syntax such as `{{if ...}}`
--   and as such have more intermediate object introduced to differentiate
--   control flow from data and interpolation.

module Kriti.ParserJson where

import qualified Kriti.Parser          as K
import qualified Data.HashMap.Internal as H
import qualified Data.Aeson            as J
import qualified Data.Aeson.Types      as J

import Data.Text (Text)
import qualified Kriti.Error as K

newtype Kontrol = Kontrol { unKontrol :: K.ValueExt }
  deriving (Show)

kure :: Applicative m => K.ValueExt -> m Kontrol
kure = pure . Kontrol

instance J.FromJSON Kontrol where
  parseJSON = \case
    J.Null                  -> kure K.Null
    J.String s              -> kure $ K.String s -- Literal
    J.Number i              -> kure $ K.Number i
    J.Bool p                -> kure $ K.Boolean p
    J.Array arr             -> Kontrol . K.Array <$> traverse J.parseJSON arr
    J.Object obj
      | (Just (J.String i)) <- H.lookup "kriti-interpretation" obj
                            -> interpret obj i
    J.Object obj | null obj -> kure K.Null
    J.Object obj            -> Kontrol . K.Object <$> traverse J.parseJSON obj

-- | Default span for use in structured data without corresponding spans
jsonSpan :: (K.SourcePosition, Maybe a)
jsonSpan = (K.SourcePosition "JSON Data" 0 0, Nothing)

binary :: J.Object
  -> ((K.SourcePosition, Maybe a)
  -> K.ValueExt -> K.ValueExt -> K.ValueExt)
  -> J.Parser Kontrol
binary o k = do
  left  <- o .: "left"
  right <- o .: "right"
  kure $ k jsonSpan left right

-- | TODO: make this more robust
quote :: Text -> Text
quote t = "\"" <> t <> "\""

stringTemplate :: Text -> J.Parser Kontrol
stringTemplate s = do
  case K.parserAndLexer (quote s) of
    Left  e -> fail $ "Couldn't parse string template: " <> show s <> " (" <> show e <> ")"
    Right v -> kure v

-- | Helper to ensure that values are interpreted as Kontrol structures instead of Aeson wrapped in Kriti
(.:) :: J.Object -> Text -> J.Parser K.ValueExt
o .: k = do
  v <- o J..: k
  pure $ unKontrol v

-- TODO: Find a way to be more precise with spans
interpret :: J.Object -> Text -> J.Parser Kontrol
interpret o = \case
  "StringInterp" -> do
    value :: J.Value <- o J..: "value"
    case value of
      J.String s -> stringTemplate s
      J.Object obj -> do
        exts <- obj J..: "exts"
        kure $ K.StringInterp jsonSpan exts
      _ -> fail "StringInterp values must be Strings (templates) or Objects {exts: [...]}"

  "Path" -> do
    accessors <- o J..: "accessors" -- TODO: Aeson instance for accessors
    kure $ K.Path $ map (jsonSpan,) accessors
    -- [(Span, Accessor)]

  "Iff" -> do
    condition <- o .: "condition" -- TODO: Use Kontrol instead of Kriti - Maybe a custom .: would help
    true      <- o .: "true"
    false     <- o .: "false"
    kure $ K.Iff jsonSpan condition true false

  "Eq"           -> binary o K.Eq
  "Gt"           -> binary o K.Gt
  "Lt"           -> binary o K.Lt
  "And"          -> binary o K.And
  "Or"           -> binary o K.Or

  "Member" -> do
    item       <- o .: "item"
    collection <- o .: "collection"
    kure $ K.Member jsonSpan item collection

  "Range" -> do
    indexVM    <- o J..: "index"
    valueV     <- o J..: "value"
    collection <- o J..: "collection"
    body       <- o .: "body"
    kure $ K.Range jsonSpan indexVM valueV (map (jsonSpan,) collection) body

  "JSON" -> do
    -- Literal JSON that is subject to no further interpretation
    v <- o J..: "value"
    j <- J.parseJSON v
    kure j

  i -> fail $ "Invalid kriti-interpretation type: " <> show i
