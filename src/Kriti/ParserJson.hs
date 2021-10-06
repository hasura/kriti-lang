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

-- TODO: Find a way to be more precise with spans
interpret :: J.Object -> Text -> J.Parser Kontrol
interpret o = \case
  "StringInterp" -> do
    value :: J.Value <- o J..: "value"
    case value of
      J.String s -> undefined -- Parse template
      J.Object o -> do
        exts <- o J..: "exts"
        kure $ K.StringInterp jsonSpan exts
      _ -> fail "StringInterp values must be Strings (templates) or Objects {exts: [...]}"

  "Path"         -> do
    accessors <- o J..: "accessors" -- TODO: Aeson instance for accessors
    kure $ K.Path $ map (jsonSpan,) accessors
    -- [(Span, Accessor)]

  "Iff"          -> do
    condition <- o J..: "condition" -- TODO: Aeson instance for accessors
    true      <- o J..: "true" -- TODO: Aeson instance for accessors
    false     <- o J..: "false" -- TODO: Aeson instance for accessors
    kure $ K.Iff jsonSpan condition true false

  "Eq"           -> undefined -- Span ValueExt ValueExt
  "Gt"           -> undefined -- Span ValueExt ValueExt
  "Lt"           -> undefined -- Span ValueExt ValueExt
  "And"          -> undefined -- Span ValueExt ValueExt
  "Or"           -> undefined -- Span ValueExt ValueExt
  "Member"       -> undefined -- Span ValueExt ValueExt
  "Range"        -> undefined -- Span (Maybe Text) Text [(Span, Accessor)] ValueExt
  "JSON"         -> do
    -- Literal JSON that is subject to no further interpretation
    v <- o J..: "value"
    j <- J.parseJSON v
    kure j
  i -> fail $ "Can't interpret kriti-interpretation type: " <> show i
