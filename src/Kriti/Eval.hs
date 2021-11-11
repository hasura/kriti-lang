module Kriti.Eval where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as J
import Data.Foldable (foldlM)
import Data.Function
import qualified Data.HashMap.Strict as M
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Vector as V
import Kriti.Error
import qualified Network.URI as URI
import Kriti.Parser.Spans
import Kriti.Parser.Token

data EvalError
  = -- | The first SourcePosition is the point where the lookup failed
    InvalidPath Span (V.Vector Accessor)
  | TypeError Span T.Text
  | RangeError Span
  deriving (Show)

instance RenderError EvalError where
  render :: EvalError -> RenderedError
  render (InvalidPath span' path) = RenderedError {_code = InvalidPathCode, _message = "Path Lookup Error: \"" <> renderPath path <> "\"", _span = span'}
  render (TypeError span' txt) = RenderedError {_code = TypeErrorCode, _message = "Type Error: " <> txt, _span = span'}
  render (RangeError span') = RenderedError {_code = RangeErrorCode, _message = "Range Error: Can only range over an array", _span = span'}

type Ctxt = M.HashMap T.Text J.Value

getSourcePos :: EvalError -> Span
getSourcePos (InvalidPath pos _) = pos
getSourcePos (TypeError pos _) = pos
getSourcePos (RangeError pos) = pos

evalPath :: J.Value -> V.Vector (Accessor) -> ExceptT EvalError (Reader Ctxt) J.Value
evalPath ctx path =
  let step :: Monad m => J.Value -> (Accessor) -> ExceptT EvalError m J.Value
      step (J.Object o) (Obj _ k) = maybe (throwError $ InvalidPath undefined path) pure $ M.lookup k o
      step (J.Array xs) (Arr _ i) = maybe (throwError $ InvalidPath undefined path) pure $ xs V.!? i
      -- TODO: Should we extend this error message with the local Context?
      step _ (Obj _ _) = throwError $ TypeError undefined "Expected object"
      step _ (Arr _ _) = throwError $ TypeError undefined "Expected array"
   in foldlM step ctx path

isString :: J.Value -> Bool
isString J.String {} = True
isString _ = False

runEval :: ValueExt -> [(T.Text, J.Value)] -> Either EvalError J.Value
runEval template source =
  let ctx = M.fromList source
   in runReader (runExceptT (eval template)) ctx

eval :: ValueExt -> ExceptT EvalError (Reader Ctxt) J.Value
eval = \case
  String _ str -> pure $ J.String str
  Number _ i -> pure $ J.Number i
  Boolean _ p -> pure $ J.Bool p
  Null _ -> pure J.Null
  Object _ fields -> J.Object <$> traverse eval fields
  StringTem _ ts -> do
    vals <- traverse eval ts
    vals & flip foldlM (J.String mempty) \(J.String acc) -> \case
      J.String val' -> pure $ J.String $ acc <> val'
      -- TODO: Improve Span Construction/Reporting for StringInterp
      _ -> throwError $ InvalidPath undefined V.empty
  Array _ xs -> J.Array <$> traverse eval xs
  Path _ path -> do
    ctx <- ask
    evalPath (J.Object ctx) path
  Iff _ p t1 t2 ->
    eval p >>= \case
      J.Bool True -> eval t1
      J.Bool False -> eval t2
      p' -> throwError $ TypeError undefined $ T.pack $ show p' <> "' is not a boolean."
  Eq _ t1 t2 -> do
    res <- (==) <$> eval t1 <*> eval t2
    pure $ J.Bool res
  Lt _ t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    pure $ J.Bool $ t1' < t2'
  Gt _ t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    pure $ J.Bool $ t1' > t2'
  And _ t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p && q
      (t1'', J.Bool _) -> throwError $ TypeError undefined $ T.pack $ show t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError undefined $ T.pack $ show t2'' <> "' is not a boolean."
  Or _ t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p || q
      (t1'', J.Bool _) -> throwError $ TypeError undefined $ T.pack $ show t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError undefined $ T.pack $ show t2'' <> "' is not a boolean."
  Member _ t ts -> do
    ts' <- eval ts
    case ts' of
      J.Array xs -> do
        t' <- eval t
        pure $ J.Bool $ t' `V.elem` xs
      _ -> throwError $ TypeError undefined $ T.pack $ show ts' <> " is not an array."
  Range _ idx binder path body -> do
    ctx <- ask
    pathResult <- evalPath (J.Object ctx) path
    case pathResult of
      J.Array arr -> fmap J.Array . flip V.imapM arr $ \i val ->
        let newScope = [(binder, val)] <> [(idxBinder, J.Number $ fromIntegral i) | idxBinder <- maybeToList idx]
         in local (M.fromList newScope <>) (eval body)
      _ -> throwError $ RangeError $ error "TODO SPANS" 
  EscapeURI _ t1 -> do
    t1' <- eval t1
    case t1' of
      J.String str ->
        let escapedUri = T.pack $ URI.escapeURIString URI.isUnreserved $ T.unpack str
         in pure $ J.String escapedUri
      _ -> throwError $ TypeError (error "TODO") $ T.pack $ show t1' <> " is not a string."
