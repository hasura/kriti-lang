module Kriti.Eval where

import Kriti.Parser (Accessor(..), ValueExt(..), renderPath)

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (foldlM)
import Data.Text (Text)
import Data.Maybe (maybeToList)


import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

type Ctxt = M.HashMap Text J.Value

evalPath :: J.Value -> [Accessor] -> ExceptT String (Reader Ctxt) J.Value
evalPath ctx path =
  let renderError path' = "InvalidPath: \"" <> renderPath path' <> "\""
      step :: Monad m => J.Value -> Accessor -> ExceptT String m J.Value
      step (J.Object o) (Obj k) = maybe (throwError $ renderError path) pure $ M.lookup k o
      step (J.Array xs) (Arr i) = maybe (throwError $ renderError path) pure $ xs V.!? i
      step _ _ = throwError $ renderError path
  in foldlM step ctx path

runEval :: ValueExt -> [(Text, J.Value)] -> Either String J.Value
runEval template source =
  let ctx = M.fromList source
  in runReader (runExceptT (eval template)) ctx

-- NOTE: In general where do we want to produce errors and where is it ok to return null?
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
      p' -> throwError $ "Type Error: '" <> show p' <> "' is not a boolean."
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
  AND t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p && q
      (t1'', J.Bool _) -> throwError $ "Type Error: '" <> show t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ "Type Error: '" <> show t2'' <> "' is not a boolean."
  OR t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p || q
      (t1'', J.Bool _) -> throwError $ "Type Error: '" <> show t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ "Type Error: '" <> show t2'' <> "' is not a boolean."
  Member t ts -> do
    ts' <- eval ts
    case ts' of
      J.Array xs -> do
        t' <- eval t
        pure $ J.Bool $ t' `V.elem` xs
      _ -> throwError $ "Type Error: " <> show ts' <> " is not an array."
  Range idx binder path body -> do
    ctx <- ask
    pathResult <- evalPath (J.Object ctx) path
    case pathResult of
      J.Array arr -> fmap J.Array . flip V.imapM arr $ \i val ->
        let newScope = [(binder, val)] <> [ (idxBinder, J.Number $ fromIntegral i) | idxBinder <- maybeToList idx ]
        in local (M.fromList newScope <>) (eval body)
      _ -> throwError "Can only range over an array"

