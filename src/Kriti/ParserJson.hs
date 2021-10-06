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

binary :: (J.FromJSON t1, J.FromJSON t2) =>
  J.Object
  -> ((K.SourcePosition, Maybe a) -> t1 -> t2 -> K.ValueExt)
  -> J.Parser Kontrol
binary o k = do
  left  <- o J..: "left"
  right <- o J..: "right"
  kure $ k jsonSpan left right

stringTemplate s = do
  case K.parserAndLexer s of
    _ -> fail "oops@!"

-- TODO: Find a way to be more precise with spans
interpret :: J.Object -> Text -> J.Parser Kontrol
interpret o = \case
  "StringInterp" -> do
    value :: J.Value <- o J..: "value"
    case value of
      J.String s -> undefined -- Parse template
      J.Object obj -> do
        exts <- obj J..: "exts"
        kure $ K.StringInterp jsonSpan exts
      _ -> fail "StringInterp values must be Strings (templates) or Objects {exts: [...]}"

  "Path" -> do
    accessors <- o J..: "accessors" -- TODO: Aeson instance for accessors
    kure $ K.Path $ map (jsonSpan,) accessors
    -- [(Span, Accessor)]

  "Iff" -> do
    condition <- o J..: "condition" -- TODO: Use Kontrol instead of Kriti - Maybe a custom .: would help
    true      <- o J..: "true"
    false     <- o J..: "false"
    kure $ K.Iff jsonSpan condition true false

  "Eq"           -> binary o K.Eq
  "Gt"           -> binary o K.Gt
  "Lt"           -> binary o K.Lt
  "And"          -> binary o K.And
  "Or"           -> binary o K.Or

  "Member" -> do
    item       <- o J..: "item"
    collection <- o J..: "collection"
    kure $ K.Member jsonSpan item collection

  "Range" -> do
    indexVM    <- o J..: "index"
    valueV     <- o J..: "value"
    collection <- o J..: "collection"
    body       <- o J..: "body"
    kure $ K.Range jsonSpan indexVM valueV (map (jsonSpan,) collection) body

  "JSON" -> do
    -- Literal JSON that is subject to no further interpretation
    v <- o J..: "value"
    j <- J.parseJSON v
    kure j

  i -> fail $ "Invalid kriti-interpretation type: " <> show i
