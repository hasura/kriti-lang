module Kriti.Eval where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as B
import Data.Foldable (foldlM)
import Data.Function
import Data.HashMap.Internal as Map
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Kriti.Aeson.Compat as Compat
import Kriti.Error
import Kriti.Parser.Spans
import Kriti.Parser.Token
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

evalFieldChain :: (ValueExt -> ExceptT EvalError (Reader Ctxt) J.Value) -> Span -> J.Object -> [Either T.Text ValueExt] -> ExceptT EvalError (Reader Ctxt) J.Value
evalFieldChain eval sp ctx path = do
  src <- asks fst
  let step :: Maybe J.Value -> Either T.Text ValueExt -> ExceptT EvalError (Reader Ctxt) (Maybe J.Value)
      step (Just (J.Object o)) (Left bndr) = pure $ Compat.lookup bndr o
      step (Just (J.Object o)) (Right trm) =
        eval trm >>= \case
          J.String bndr -> pure $ Compat.lookup bndr o
          _json -> pure Nothing
      step (Just (J.Array xs)) (Right trm) =
        eval trm >>= \case
          J.Number n -> case Scientific.toBoundedInteger @Int n of
            Just i -> pure $ xs V.!? i
            Nothing -> pure Nothing
          _json -> pure Nothing
      step (Just json) _ = throwError $ TypeError src sp $ renderBL $ "'" <> J.encode json <> "' is not an Object."
      step Nothing _ = pure Nothing
  fmap (fromMaybe (J.Null)) $ foldlM step (Just $ J.Object ctx) path

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

typeOfJSON :: J.Value -> T.Text
typeOfJSON J.Object {} = "Object"
typeOfJSON J.Array {} = "Array"
typeOfJSON J.String {} = "String"
typeOfJSON J.Number {} = "Number"
typeOfJSON J.Bool {} = "Boolean"
typeOfJSON J.Null = "Null"

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
  Var sp bndr -> do
    (src, ctx) <- ask
    maybe (throwError $ TypeError src sp "Key not found TODO IMPROVE THIS ERROR MSG") pure $ Compat.lookup bndr ctx
  RequiredFieldAccess sp t1 (Left bndr) -> do
    src <- asks fst
    eval t1 >>= \case
      J.Object km ->
        maybe (throwError $ TypeError src sp "Key not found TODO IMPROVE THIS ERROR MSG") pure $ Compat.lookup bndr km
      json -> throwError $ TypeError src (locate t1) $ "Couldn't match '" <> typeOfJSON json <> "' with 'Object'."
  RequiredFieldAccess sp t1 (Right t2) -> do
    src <- asks fst
    eval t1 >>= \case
      J.Object km -> do
        eval t2 >>= \case
          J.String bndr ->
            maybe (throwError $ TypeError src sp "Key not found TODO IMPROVE THIS ERROR MSG") pure $ Compat.lookup bndr km
          json -> throwError $ TypeError src (locate t1) $ "Couldn't match '" <> typeOfJSON json <> "' with 'String'."
      J.Array xs -> do
        eval t2 >>= \case
          J.Number n -> case Scientific.toBoundedInteger @Int n of
            Just i -> maybe (throwError $ RangeError src sp) pure $ xs V.!? i
            Nothing -> throwError $ TypeError src sp $ "Unexpected Float '" <> T.pack (show n) <> "', expected an integer."
          json -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode json <> "' is not a Number."
      json -> throwError $ TypeError src (locate t1) $ "Couldn't match '" <> typeOfJSON json <> "' with 'Object'."
  OptionalFieldAccess sp t1 fields -> do
    src <- asks fst
    eval t1 >>= \case
      J.Object km -> evalFieldChain eval sp km fields
      --J.Array arr -> evalFieldChain eval sp arr fields
      json -> throwError $ TypeError src (locate t1) $ "Couldn't match '" <> typeOfJSON json <> "' with 'Object'."
  Iff sp p t1 t2 -> do
    src <- asks fst
    eval p >>= \case
      J.Bool True -> eval t1
      J.Bool False -> eval t2
      p' -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode p' <> "' is not a Boolean."
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
      (json, J.Bool _) -> throwError $ TypeError src sp $ "Couldn't match '" <> typeOfJSON json <> "' with 'Boolean'."
      (_, json) -> throwError $ TypeError src sp $ "Couldn't match '" <> typeOfJSON json <> "' with 'Boolean'."
  Or sp t1 t2 -> do
    src <- asks fst
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p || q
      (json, J.Bool _) -> throwError $ TypeError src sp $ "Couldn't match '" <> typeOfJSON json <> "' with 'Boolean'."
      (_, json) -> throwError $ TypeError src sp $ "Couldn't match '" <> typeOfJSON json <> "' with 'Boolean'."
  In sp t1 t2 -> do
    src <- asks fst
    v1 <- eval t1
    v2 <- eval t2
    case (v1, v2) of
      (J.String key, J.Object fields) -> do
        let fields' = fmap fst $ Compat.toList fields
        pure $ J.Bool $ key `elem` fields'
      (json, J.Object _) -> throwError $ TypeError src sp $ "Couldn't match '" <> typeOfJSON json <> "' with 'String'."
      (_, J.Array vals) -> pure $ J.Bool $ v1 `V.elem` vals
      (_, json) -> throwError $ TypeError src sp $ "Couldn't match '" <> typeOfJSON json <> "' with 'Array'."
  Range sp idx binder t1 body -> do
    src <- asks fst
    eval t1 >>= \case
      J.Array arr -> fmap J.Array . flip V.imapM arr $ \i val ->
        let newScope = [(binder, val)] <> [(idxBinder, J.Number $ fromIntegral i) | idxBinder <- maybeToList idx]
         in local (\(_, bndrs) -> (src, Compat.fromList newScope <> bndrs)) (eval body)
      json -> throwError $ TypeError src sp $ "Couldn't match '" <> typeOfJSON json <> "' with 'Array'."
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
