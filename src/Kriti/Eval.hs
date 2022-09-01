module Kriti.Eval where

--------------------------------------------------------------------------------

import Control.Lens (view, _1, _2, _3)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.UTF8 qualified as B
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.HashMap.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Kriti.Aeson.Compat qualified as Compat
import Kriti.Error
import Kriti.Parser.Spans
import Kriti.Parser.Token
import Prettyprinter as P
import qualified Data.Scientific as Scientific

--------------------------------------------------------------------------------

data EvalError
  = InvalidPath B.ByteString Span T.Text
  | TypeError B.ByteString Span T.Text
  | RangeError B.ByteString Span
  | ApError B.ByteString Span T.Text
  deriving (Show)

instance Pretty EvalError where
  pretty = \case
    InvalidPath src term _ -> mkPretty src "Invalid path lookup" term
    TypeError src term msg -> mkPretty src msg term
    RangeError src term -> mkPretty src "Index out of range" term
    ApError src term msg -> mkPretty src msg term
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
  serialize (InvalidPath _ term path) = SerializedError {_code = InvalidPathCode, _message = "\"" <> path <> "\"", _span = locate term}
  serialize (TypeError _ term msg) = SerializedError {_code = TypeErrorCode, _message = msg, _span = locate term}
  serialize (RangeError _ term) = SerializedError {_code = RangeErrorCode, _message = "Can only range over an array", _span = locate term}
  serialize (ApError _ term msg) = SerializedError {_code = FunctionErrorCode, _message = msg, _span = locate term}

instance Located EvalError where
  locate = \case
    InvalidPath _ pos _ -> locate pos
    TypeError _ term _ -> locate term
    RangeError _ pos -> locate pos
    ApError _ term _ -> locate term

--------------------------------------------------------------------------------

type BindingContext = Compat.Object J.Value

type ApContext = Map.HashMap T.Text (J.Value -> Either CustomFunctionError J.Value)

type Ctxt = (B.ByteString, BindingContext, ApContext)

getSource :: ExceptT EvalError (Reader Ctxt) B.ByteString
getSource = view _1

getBindings :: ExceptT EvalError (Reader Ctxt) BindingContext
getBindings = view _2

getAps :: ExceptT EvalError (Reader Ctxt) ApContext
getAps = view _3

--------------------------------------------------------------------------------

isString :: J.Value -> Bool
isString J.String {} = True
isString _ = False

--------------------------------------------------------------------------------

runEval :: B.ByteString -> ValueExt -> [(T.Text, J.Value)] -> Either EvalError J.Value
runEval src template source =
  let ctx = Compat.fromList source
   in runReader (runExceptT (eval template)) (src, ctx, mempty)

runEvalWith :: B.ByteString -> ValueExt -> [(T.Text, J.Value)] -> Map.HashMap T.Text (J.Value -> Either CustomFunctionError J.Value) -> Either EvalError J.Value
runEvalWith src template source funcMap =
  let ctx = Compat.fromList source
   in runReader (runExceptT (eval template)) (src, ctx, funcMap)

--------------------------------------------------------------------------------

typeOfJSON :: J.Value -> T.Text
typeOfJSON J.Object {} = "Object"
typeOfJSON J.Array {} = "Array"
typeOfJSON J.String {} = "String"
typeOfJSON J.Number {} = "Number"
typeOfJSON J.Bool {} = "Boolean"
typeOfJSON J.Null = "Null"

--------------------------------------------------------------------------------

eval :: ValueExt -> ExceptT EvalError (Reader Ctxt) J.Value
eval = \case
  Var sp bndr -> do
    src <- getSource
    ctx <- getBindings
    maybe (throwError (InvalidPath src sp "TODO")) pure $ Compat.lookup bndr ctx
  Ap sp (Var _ fName) t1 -> do
    src <- getSource
    funcMap <- getAps
    v1 <- eval t1
    case Map.lookup fName funcMap of
      Nothing -> throwError $ ApError src sp $ "Ap " <> fName <> " is not defined."
      Just f -> case f v1 of
        Left ee -> throwError $ ApError src (locate t1) $ unwrapError ee
        Right va -> pure va
  Ap sp _t1 _t2 -> do
    src <- getSource
    throwError $ ApError src sp "Type mismatch in function application. (fix this message)"
  String _ str -> pure $ J.String str
  StringTem sp ts -> do
    src <- view _1
    vals <- traverse eval ts
    str <-
      vals & flip foldlM mempty \acc -> \case
        J.String val' -> pure $ acc <> val'
        J.Number i -> pure $ acc <> TE.decodeUtf8 (BL.toStrict $ J.encode i)
        J.Bool p -> pure $ acc <> TE.decodeUtf8 (BL.toStrict $ J.encode p)
        t -> throwError $ TypeError src sp $ "Cannot interpolate type: '" <> typeOfJSON t <> "'."
    pure $ J.String str
  Number _ i -> pure $ J.Number i
  Boolean _ p -> pure $ J.Bool p
  Null _ -> pure J.Null
  Object _ fields -> J.Object <$> traverse eval fields
  Array _ xs -> J.Array <$> traverse eval xs
  Field sp opt t1 t2 -> do
    src <- getSource
    v1 <- eval t1
    v2 <- eval t2
    case (v1, v2) of
      (J.Object km, J.String bndr) -> 
        let v1' = Compat.lookup bndr km
        in case opt of
          Optional -> maybe (pure J.Null) pure v1'
          NotOptional -> maybe (throwError (InvalidPath src sp bndr)) pure v1'
      (J.Array arr, J.Number n) -> 
        case Scientific.toBoundedInteger @Int n of
          Just i ->
            let t1' = arr V.!? i
            in case opt of
              Optional -> maybe (pure J.Null) pure t1'
              NotOptional -> maybe (throwError (RangeError src sp)) pure t1'
          Nothing -> throwError $ TypeError src sp $ "Unexpected Float '" <> T.pack (show n) <> "', expected an integer."
      (J.Object _, _) -> throwError $ TypeError src sp $ "Unexpected type '" <> typeOfJSON v2 <> "', expected a string."
      (J.Array _, _) -> throwError $ TypeError src sp $ "Unexpected type '" <> typeOfJSON v2 <> "', expected an integer."
      _ -> throwError $ TypeError src sp $ "Unexpected type '" <> typeOfJSON v1 <> "' expected an Object or an Array."
  Iff sp p t1 t2 -> do
    src <- getSource
    eval p >>= \case
      J.Bool True -> eval t1
      J.Bool False -> eval t2
      p' -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode p' <> "' is not a boolean."
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
    src <- getSource
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p && q
      (t1'', J.Bool _) -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode t2'' <> "' is not a boolean."
  Or sp t1 t2 -> do
    src <- getSource
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p || q
      (t1'', J.Bool _) -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode t1'' <> "' is not a boolean."
      (_, t2'') -> throwError $ TypeError src sp $ renderBL $ "'" <> J.encode t2'' <> "' is not a boolean."
  In sp t1 t2 -> do
    src <- getSource
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
    src <- getSource
    pathResult <- eval path
    case pathResult of
      J.Array arr -> fmap J.Array . flip V.imapM arr $ \i val ->
        let newScope = [(binder, val)] <> [(idxBinder, J.Number $ fromIntegral i) | idxBinder <- maybeToList idx]
         in local (\(_, bndrs, funcs) -> (src, Compat.fromList newScope <> bndrs, funcs)) (eval body)
      _ -> throwError $ RangeError src sp
  Defaulting _ t1 t2 -> do
    v1 <- eval t1
    case v1 of
      J.Null -> eval t2
      json -> pure json
