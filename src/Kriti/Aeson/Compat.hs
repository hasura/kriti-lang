{-# LANGUAGE CPP #-}

module Kriti.Aeson.Compat where

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
#else
import qualified Data.HashMap.Strict as M
#endif
import qualified Data.Text as T

type Object v =
#if MIN_VERSION_aeson(2,0,0)
  KM.KeyMap v
#else
  M.HashMap T.Text v
#endif

fromList :: [(T.Text, v)] -> Object v
fromList =
#if MIN_VERSION_aeson(2,0,0)
  KM.fromList . map (first K.fromText)
#else
  M.fromList
#endif

lookup :: T.Text -> Object v -> Maybe v
lookup =
#if MIN_VERSION_aeson(2,0,0)
  KM.lookup . K.fromText
#else
  M.lookup
#endif
