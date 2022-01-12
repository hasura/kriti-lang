{-# LANGUAGE CPP #-}

module Kriti.Aeson.Compat where

#if MIN_VERSION_aeson(2,0,0)

import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import qualified Data.Text as T

type Object v = KM.KeyMap v

fromList :: [(T.Text, v)] -> Object v
fromList = KM.fromList . map (first K.fromText)

toList :: Object v -> [(T.Text, v)]
toList = map (first K.toText) . KM.toList

lookup :: T.Text -> Object v -> Maybe v
lookup = KM.lookup . K.fromText

#else

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

type Object v = M.HashMap T.Text v

fromList :: [(T.Text, v)] -> Object v
fromList = M.fromList

toList :: Object v -> [(T.Text, v)]
toList = M.toList

lookup :: T.Text -> Object v -> Maybe v
lookup = M.lookup

#endif
