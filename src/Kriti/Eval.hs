module Kriti.Eval where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (foldlM)
import Data.Function
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Kriti.Aeson.Compat as Compat
import Kriti.Error
import Kriti.Parser.Spans
import Kriti.Parser.Token
import qualified Network.URI as URI

data EvalError
  = InvalidPath Span (V.Vector Accessor)
  | TypeError ValueExt T.Text 
  | RangeError Span
  deriving Show

instance SerializeError EvalError where
  serialize :: EvalError -> SerializedError 
  serialize (InvalidPath span' path) = SerializedError  {_code = InvalidPathCode, _message = "\"" <> renderVect path <> "\"", _span = span'}
  serialize (TypeError term msg) = SerializedError  {_code = TypeErrorCode, _message = msg, _span = locate term}
  serialize (RangeError span') = SerializedError  {_code = RangeErrorCode, _message = "Can only range over an array", _span = span'}

type Ctxt = Compat.Object J.Value

getSourcePos :: EvalError -> Span
getSourcePos (InvalidPath pos _) = pos
getSourcePos (TypeError term _) = locate term
getSourcePos (RangeError pos) = pos

evalPath :: ValueExt -> J.Value -> V.Vector (Accessor) -> ExceptT EvalError (Reader Ctxt) J.Value
evalPath term ctx path =
  let step :: Monad m => J.Value -> Accessor -> ExceptT EvalError m J.Value
      step (J.Object o) (Obj sp k _) = maybe (throwError $ InvalidPath sp path) pure $ Compat.lookup k o
      step (J.Array xs) (Arr sp i) = maybe (throwError $ InvalidPath sp path) pure $ xs V.!? i
      -- TODO: Should we extend this error message with the local Context?
      step _ (Obj _ _ _) = throwError $ TypeError term "Expected object"
      step _ (Arr _ _) = throwError $ TypeError term "Expected array"
   in foldlM step ctx path

isString :: J.Value -> Bool
isString J.String {} = True
isString _ = False

runEval :: ValueExt -> [(T.Text, J.Value)] -> Either EvalError J.Value
runEval template source =
  let ctx = Compat.fromList source
   in runReader (runExceptT (eval template)) ctx

typoOfJSON :: J.Value -> T.Text
typoOfJSON J.Object {} = "Object"
typoOfJSON J.Array {} = "Array"
typoOfJSON J.String {} = "String"
typoOfJSON J.Number {} = "Number"
typoOfJSON J.Bool {} = "Boolean"
typoOfJSON J.Null = "Null"

eval :: ValueExt -> ExceptT EvalError (Reader Ctxt) J.Value
eval term = case term of
  String _ str -> pure $ J.String str
  Number _ i -> pure $ J.Number i
  Boolean _ p -> pure $ J.Bool p
  Null _ -> pure J.Null
  Object _ fields -> J.Object <$> traverse eval fields
  StringTem _ ts -> do
    vals <- traverse eval ts
    str <-
      vals & flip foldlM mempty \acc -> \case
        J.String val' -> pure $ acc <> val'
        J.Number i -> pure $ acc <> TE.decodeUtf8 (BL.toStrict $ J.encode i)
        J.Bool p -> pure $ acc <> TE.decodeUtf8 (BL.toStrict $ J.encode p)
        t -> throwError $ TypeError term $ "Cannot interpolate type: '" <> typoOfJSON t <> "'."
    pure $ J.String str
  Array _ xs -> J.Array <$> traverse eval xs
  Path _ path -> do
    ctx <- ask
    evalPath term (J.Object ctx) path
  Iff _ p t1 t2 ->
    eval p >>= \case
      J.Bool True -> eval t1
      J.Bool False -> eval t2
      p' -> throwError $ TypeError term $ renderBL $ "'" <> J.encode p' <> "' is not a boolean."
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
      (t1'', J.Bool _) -> throwError $ TypeError term $ renderBL $ "'" <> J.encode t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError term $ renderBL $ "'" <> J.encode t2'' <> "' is not a boolean."
  Or _ t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p || q
      (t1'', J.Bool _) -> throwError $ TypeError term $ renderBL $ "'" <> J.encode t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError term $ renderBL $ "'" <> J.encode t2'' <> "' is not a boolean."
  Member _ t ts -> do
    ts' <- eval ts
    case ts' of
      J.Array xs -> do
        t' <- eval t
        pure $ J.Bool $ t' `V.elem` xs
      _ -> throwError $ TypeError term $ T.pack $ "'" <> show ts' <> "' is not an array."
  Range sp idx binder path body -> do
    ctx <- ask
    pathResult <- evalPath term (J.Object ctx) path 
    case pathResult of
      J.Array arr -> fmap J.Array . flip V.imapM arr $ \i val ->
        let newScope = [(binder, val)] <> [(idxBinder, J.Number $ fromIntegral i) | idxBinder <- maybeToList idx]
         in local (Compat.fromList newScope <>) (eval body)
      _ -> throwError $ RangeError sp
  EscapeURI _ t1 -> do
    t1' <- eval t1
    case t1' of
      J.String str ->
        let escapedUri = T.pack $ URI.escapeURIString URI.isUnreserved $ T.unpack str
         in pure $ J.String escapedUri
      _ -> throwError $ TypeError term $ T.pack $ "'" <> show t1' <> "' is not a string."
