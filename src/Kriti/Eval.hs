module Kriti.Eval where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable        (foldlM)
import           Data.Maybe           (maybeToList)
import           Kriti.Error
import           Kriti.Parser         (Accessor(..), ValueExt(..), renderPath)

import qualified Data.Aeson           as J
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T
import qualified Data.Vector          as V

data EvalError =
    InvalidPath Span [(Span, Accessor)]
  -- ^ The first SourcePosition is the point where the lookup failed
  | TypeError Span T.Text
  | RangeError Span
  deriving Show

instance RenderError EvalError where
  render :: EvalError -> RenderedError
  render (InvalidPath span' path) = RenderedError { _code = InvalidPathCode, _message = "Path Lookup Error: \"" <> renderPath path <> "\"", _span = span' }
  render (TypeError span' txt)    = RenderedError { _code = TypeErrorCode,   _message = "Type Error: " <> txt, _span = span' }
  render (RangeError span')       = RenderedError { _code = RangeErrorCode,  _message = "Range Error: Can only range over an array", _span = span' }

type Ctxt = M.HashMap T.Text J.Value

getSourcePos :: EvalError -> Span
getSourcePos (InvalidPath pos _) = pos
getSourcePos (TypeError pos _)   = pos
getSourcePos (RangeError pos)    = pos

evalPath :: J.Value -> [(Span, Accessor)] -> ExceptT EvalError (Reader Ctxt) J.Value
evalPath ctx path =
  let step :: Monad m => J.Value -> (Span, Accessor) -> ExceptT EvalError m J.Value
      step (J.Object o) (pos, Obj k) = maybe (throwError $ InvalidPath pos path) pure $ M.lookup k o
      step (J.Array xs) (pos, Arr i) = maybe (throwError $ InvalidPath pos path) pure $ xs V.!? i
      -- TODO: Should we extend this error message with the local Context?
      step _ (pos, Obj _) = throwError $ TypeError pos "Expected object"
      step _ (pos, Arr _) = throwError $ TypeError pos "Expected array"
  in foldlM step ctx path

runEval :: ValueExt -> [(T.Text, J.Value)] -> Either EvalError J.Value
runEval template source =
  let ctx = M.fromList source
  in runReader (runExceptT (eval template)) ctx

-- NOTE: In general where do we want to produce errors and where is it ok to return null?
eval :: ValueExt -> ExceptT EvalError (Reader Ctxt) J.Value
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
  Iff pos p t1 t2 ->
    eval p >>= \case
      J.Bool True -> eval t1
      J.Bool False -> eval t2
      p' -> throwError $ TypeError pos $ T.pack $ show p' <> "' is not a boolean."
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
  AND pos t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p && q
      (t1'', J.Bool _) -> throwError $ TypeError pos $ T.pack $ show t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError pos $ T.pack $ show t2'' <> "' is not a boolean."
  OR pos t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p || q
      (t1'', J.Bool _) -> throwError $ TypeError pos $ T.pack $ show t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError pos $ T.pack $ show t2'' <> "' is not a boolean."
  Member pos t ts -> do
    ts' <- eval ts
    case ts' of
      J.Array xs -> do
        t' <- eval t
        pure $ J.Bool $ t' `V.elem` xs
      _ -> throwError $ TypeError pos $ T.pack $ show ts' <> " is not an array."
  Range pos idx binder path body -> do
    ctx <- ask
    pathResult <- evalPath (J.Object ctx) path
    case pathResult of
      J.Array arr -> fmap J.Array . flip V.imapM arr $ \i val ->
        let newScope = [(binder, val)] <> [ (idxBinder, J.Number $ fromIntegral i) | idxBinder <- maybeToList idx ]
        in local (M.fromList newScope <>) (eval body)
      _ -> throwError $ RangeError pos