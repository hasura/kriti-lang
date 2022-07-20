module Kriti.Eval where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as B
import Data.Foldable (foldlM)
import Data.Function
import Data.HashMap.Internal as Map
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Kriti.Aeson.Compat as Compat
import Kriti.Error
import Kriti.Parser.Spans
import Kriti.Parser.Token
import qualified Network.URI as URI
import Prettyprinter as P

data EvalError
  = InvalidPath B.ByteString Span (V.Vector Accessor)
  | TypeError B.ByteString Span T.Text
  | RangeError B.ByteString Span
  | FunctionError B.ByteString Span T.Text
  deriving (Show)

instance Pretty EvalError where
  pretty = \case
    InvalidPath src term _ -> mkPretty src "Invalid path lookup" term
    TypeError src term msg -> mkPretty src msg term
    RangeError src term -> mkPretty src "Index out of range" term
    FunctionError src term msg -> mkPretty src msg term
    where
      mkPretty source msg term =
        let AlexSourcePos {line = startLine, col = startCol} = start $ locate term
            AlexSourcePos {col = endCol} = end $ locate term
            sourceLine = B.lines source !! startLine
         in vsep
              [ "Runtime Error:",
                indent 2 $ pretty (msg :: T.Text),
                indent 4 "|",
                indent 2 $ pretty startLine <+> "|" <+> pretty (TE.decodeUtf8 sourceLine),
                indent 4 $ "|" <> indent (startCol) (pretty $ replicate (endCol - startCol) '^')
              ]

instance SerializeError EvalError where
  serialize :: EvalError -> SerializedError
  serialize (InvalidPath _ term path) = SerializedError {_code = InvalidPathCode, _message = "\"" <> renderVect path <> "\"", _span = locate term}
  serialize (TypeError _ term msg) = SerializedError {_code = TypeErrorCode, _message = msg, _span = locate term}
  serialize (RangeError _ term) = SerializedError {_code = RangeErrorCode, _message = "Can only range over an array", _span = locate term}
  serialize (FunctionError _ term msg) = SerializedError {_code = FunctionErrorCode, _message = msg, _span = locate term}

type Ctxt = (B.ByteString, Compat.Object J.Value)

getSourcePos :: EvalError -> Span
getSourcePos (InvalidPath _ pos _) = locate pos
getSourcePos (TypeError _ term _) = locate term
getSourcePos (RangeError _ pos) = locate pos
getSourcePos (FunctionError _ term _) = locate term

evalPath :: Span -> J.Value -> V.Vector (Accessor) -> ExceptT EvalError (Reader Ctxt) J.Value
evalPath sp ctx path = do
  src <- asks fst
  let maybeThrow NotOptional = maybe (throwError $ Left $ InvalidPath src sp path) pure
      maybeThrow Optional = maybe (throwError $ Right ()) pure
      step :: J.Value -> Accessor -> ExceptT (Either EvalError ()) (Reader Ctxt) J.Value
      step (J.Object o) (Obj _ optional k _) = maybeThrow optional $ Compat.lookup k o
      step (J.Array xs) (Arr _ optional i) = maybeThrow optional $ xs V.!? i
      step _ (Obj _ _ _ _) = throwError $ Left $ TypeError src sp "Expected object"
      step _ (Arr _ _ _) = throwError $ Left $ TypeError src sp "Expected array"
   in mapExceptT (fmap $ either (either throwError (pure . const J.Null)) pure) $ foldlM step ctx path

isString :: J.Value -> Bool
isString J.String {} = True
isString _ = False

runEval :: B.ByteString -> ValueExt -> [(T.Text, J.Value)] -> Either EvalError J.Value
runEval src template source =
  let ctx = Compat.fromList source
   in runReader (runExceptT (evalWith Map.empty template)) (src, ctx)

runEvalWith :: B.ByteString -> ValueExt -> [(T.Text, J.Value)] -> Map.HashMap T.Text (J.Value -> Either CustomFunctionError J.Value) -> Either EvalError J.Value
runEvalWith src template source funcMap =
  let ctx = Compat.fromList source
   in runReader (runExceptT (evalWith funcMap template)) (src, ctx)

typoOfJSON :: J.Value -> T.Text
typoOfJSON J.Object {} = "Object"
typoOfJSON J.Array {} = "Array"
typoOfJSON J.String {} = "String"
typoOfJSON J.Number {} = "Number"
typoOfJSON J.Bool {} = "Boolean"
typoOfJSON J.Null = "Null"

evalWith :: Map.HashMap T.Text (J.Value -> Either CustomFunctionError J.Value) -> ValueExt -> ExceptT EvalError (Reader Ctxt) J.Value
evalWith funcMap = \case
  String _ str -> pure $ J.String str
  Number _ i -> pure $ J.Number i
  Boolean _ p -> pure $ J.Bool p
  Null _ -> pure J.Null
  Object _ fields -> J.Object <$> traverse eval fields
  StringTem _sp ts -> do
    vals <- traverse eval ts
    str <-
      vals & flip foldlM mempty \acc -> \case
        J.String val' -> pure $ acc <> val'
        json -> pure $ acc <> TE.decodeUtf8 (BL.toStrict $ J.encode json)
    pure $ J.String str
  Array _ xs -> J.Array <$> traverse eval xs
  Path sp path -> do
    ctx <- asks snd
    evalPath sp (J.Object ctx) path
  Iff sp p t1 t2 -> do
    src <- asks fst
    eval p >>= \case
      J.Bool True -> eval t1
      J.Bool False -> eval t2
      p' -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode p' <> "' is not a boolean."
  Not sp t1 -> do
    src <- asks fst
    v1 <- eval t1
    case v1 of
      J.Bool p -> pure $ J.Bool $ not p
      _ -> throwError $ TypeError src sp $ T.pack $ show t1 <> "is not a boolean."
  Eq _ t1 t2 -> do
    res <- (==) <$> eval t1 <*> eval t2
    pure $ J.Bool res
  NotEq _ t1 t2 -> do
    res <- (/=) <$> eval t1 <*> eval t2
    pure $ J.Bool res
  Lt _ t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    pure $ J.Bool $ t1' < t2'
  Gt _ t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    pure $ J.Bool $ t1' > t2'
  Lte _ t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    pure $ J.Bool $ t1' <= t2'
  Gte _ t1 t2 -> do
    t1' <- eval t1
    t2' <- eval t2
    pure $ J.Bool $ t1' >= t2'
  And sp t1 t2 -> do
    src <- asks fst
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p && q
      (t1'', J.Bool _) -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode t2'' <> "' is not a boolean."
  Or sp t1 t2 -> do
    src <- asks fst
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p || q
      (t1'', J.Bool _) -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode t2'' <> "' is not a boolean."
  In sp t1 t2 -> do
    src <- asks fst
    v1 <- eval t1
    v2 <- eval t2
    case (v1, v2) of
      (J.String key, J.Object fields) -> do
        let fields' = fmap fst $ Compat.toList fields
        pure $ J.Bool $ key `elem` fields'
      (json, J.Object _) -> throwError $ TypeError src sp $ T.pack $ show json <> " is not a String."
      (_, J.Array vals) -> pure $ J.Bool $ v1 `V.elem` vals
      (_, json) -> throwError $ TypeError src sp $ T.pack $ show json <> " is not an Object or Array."
  Range sp idx binder path body -> do
    (src, ctx) <- ask
    pathResult <- evalPath sp (J.Object ctx) path
    case pathResult of
      J.Array arr -> fmap J.Array . flip V.imapM arr $ \i val ->
        let newScope = [(binder, val)] <> [(idxBinder, J.Number $ fromIntegral i) | idxBinder <- maybeToList idx]
         in local (\(_, bndrs) -> (src, Compat.fromList newScope <> bndrs)) (eval body)
      _ -> throwError $ RangeError src sp
  EscapeURI sp t1 -> do
    src <- asks fst
    t1' <- eval t1
    case t1' of
      J.String str ->
        let escapedUri = T.pack $ URI.escapeURIString URI.isUnreserved $ T.unpack str
         in pure $ J.String escapedUri
      _ -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode t1' <> "' is not a string."
  Function sp fName t1 -> do
    src <- asks fst
    v1 <- eval t1
    case Map.lookup fName funcMap of
      Nothing -> throwError $ FunctionError src sp $ "Function " <> fName <> " is not defined."
      Just f -> case f v1 of
        Left ee -> throwError $ FunctionError src (locate t1) $ unwrapError ee
        Right va -> pure va
  Defaulting _ t1 t2 -> do
    v1 <- eval t1
    case v1 of
      J.Null -> eval t2
      json -> pure json
  where
    eval = evalWith funcMap
