{-# OPTIONS_GHC -Wno-orphans #-}
module Kriti.Aeson.Pretty where

----------------------------------------------------------------------

import qualified Data.Aeson as J
import qualified Data.Vector as V
import qualified Kriti.Aeson.Compat as Compat
import Prettyprinter

----------------------------------------------------------------------

instance Pretty J.Value where
  pretty = \case
    J.Object km -> object $ Compat.toList $ km
    J.Array arr -> list $ fmap pretty $ V.toList arr
    J.String str -> pretty str
    J.Number n -> pretty $ show n
    J.Bool True -> "true"
    J.Bool False -> "false"
    J.Null -> "null"

object :: (Pretty k, Pretty v) => [(k, v)] -> Doc ann
object =
  group
    . encloseSep
      (flatAlt "{ " "{")
      (flatAlt " }" "}")
      ", "
    . fmap (\(k, v) -> pretty k <> colon <+> pretty v)
