module GoBasic.Eval where

import GoBasic.Parser (Accessor(..), ValueExt(..))

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import Data.Maybe (fromMaybe, maybeToList)


import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

type Ctxt = M.HashMap Text J.Value

evalPath :: J.Value -> [Accessor] -> ExceptT String (Reader Ctxt) J.Value
evalPath ctx path =
    let step :: J.Value -> Accessor -> ExceptT String (Reader Ctxt) J.Value
        step (J.Object o) (Obj k) = pure $ fromMaybe J.Null $ M.lookup k o
        step (J.Array xs) (Arr i) = pure $ fromMaybe J.Null $ xs V.!? i
        step _ _ = pure J.Null
    in foldM step ctx path

eval :: ValueExt -> ExceptT String (Reader Ctxt) J.Value
eval = \case
  String str -> pure $ J.String str
  Number i -> pure $ J.Number i
  Boolean p -> pure $ J.Bool p
  Null -> pure J.Null
  Object fields -> J.Object <$> traverse eval fields
  Array xs -> J.Array <$> traverse eval xs
  Path path -> do
    ctx <- ask
    evalPath (J.Object ctx) path
  Iff p t1 t2 ->
    eval p >>= \case
      J.Bool True -> eval t1
      J.Bool False -> eval t2
  Eq t1 t2 -> do
    res <- (==) <$> eval t1 <*> eval t2
    pure $ J.Bool res
  Lt t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    pure $ J.Bool $ t1' < t2'
  Gt t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    pure $ J.Bool $ t1' > t2'
  Range idx binder path body -> do
    ctx <- ask
    xs <- evalPath (J.Object ctx) path
    case xs of
      J.Array arr -> fmap J.Array . flip V.imapM arr $ \i val ->
        let newScope = [(binder, val)] <> [ (idxBinder, J.Number $ fromIntegral i) | idxBinder <- maybeToList idx ]
        in local (M.fromList newScope <>) (eval body)
      _ -> throwError "Can only range over an array"
  _ -> throwError "the impossible occurred"

