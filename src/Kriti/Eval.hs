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
  = AttributeError B.ByteString Span T.Text
  | NameError B.ByteString Span T.Text
  | TypeError B.ByteString Span J.Value T.Text
  | IndexError B.ByteString Span
  | FunctionError B.ByteString Span T.Text
  deriving (Show)

instance Pretty EvalError where
  pretty = \case
    AttributeError src sp bndr -> mkPretty src ("'Object' has no attribute '" <> bndr <> "'") sp
    NameError src sp var -> mkPretty src ("Variable '" <> var <> "' not in scope") sp
    TypeError src sp val expected -> mkPretty src ("Couldn't matche expected type '" <> expected <> "' with actual type '" <> typeOfJSON val <> "'") sp
    IndexError src sp -> mkPretty src "Index out of range" sp
    FunctionError src sp msg -> mkPretty src msg sp
    where
      mkPretty source msg sp =
        let AlexSourcePos {line = startLine, col = startCol} = start sp
            AlexSourcePos {col = endCol} = end sp
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
  serialize (AttributeError _ sp var) = SerializedError {_code = AttributeErrorCode, _message = "'Object' has no attritubte \'" <> var <> "\'.", _span = sp}
  serialize (NameError _ sp var) = SerializedError {_code = NameErrorCode, _message = "Variable \'" <> var <> "\' not in scope", _span = sp}
  serialize (TypeError _ term val expected) = SerializedError {_code = TypeErrorCode, _message = ("Couldn't matche expected type '" <> expected <> "' with actual type '" <> typeOfJSON val <> "'"), _span = locate term}
  serialize (IndexError _ term) = SerializedError {_code = IndexErrorCode, _message = "Can only range over an array", _span = locate term}
  serialize (FunctionError _ term msg) = SerializedError {_code = FunctionErrorCode, _message = msg, _span = locate term}

-- | The original source (for error messages) and the binding context.
type Ctxt = (B.ByteString, Compat.Object J.Value)

getSourcePos :: EvalError -> Span
getSourcePos (AttributeError _ pos _) = locate pos
getSourcePos (NameError _ pos _) = locate pos
getSourcePos (TypeError _ pos _ _) = locate pos
getSourcePos (IndexError _ pos) = locate pos
getSourcePos (FunctionError _ term _) = locate term

-- | Step through a chain of optional lookups.
evalFieldChain :: J.Value -> [J.Value] -> Maybe J.Value
evalFieldChain ctx path = do
  let step :: J.Value -> J.Value -> (Maybe J.Value)
      step (J.String bndr) ((J.Object o)) = Compat.lookup bndr o
      step (J.Number n) ((J.Array xs)) =
        Scientific.toBoundedInteger @Int n >>= \i -> xs V.!? i
      step _ _ = Nothing
  Prelude.foldr (\c -> (>>= step c)) (Just ctx) path

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
  Var sp bndr -> do
    (src, ctx) <- ask
    maybe (throwError $ NameError src sp bndr) pure $ Compat.lookup bndr ctx
  RequiredFieldAccess sp t1 (Left bndr) -> do
    src <- asks fst
    eval t1 >>= \case
      J.Object km ->
        maybe (throwError $ AttributeError src sp bndr) pure $ Compat.lookup bndr km
      json -> throwError $ TypeError src (locate t1) json "Object"
  RequiredFieldAccess sp t1 (Right t2) -> do
    src <- asks fst
    eval t1 >>= \case
      J.Object km -> do
        eval t2 >>= \case
          J.String bndr ->
            maybe (throwError $ AttributeError src sp bndr) pure $ Compat.lookup bndr km
          json -> throwError $ TypeError src (locate t1) json "String"
      J.Array xs -> do
        eval t2 >>= \case
          json@(J.Number n) -> case Scientific.toBoundedInteger @Int n of
            Just i -> maybe (throwError $ IndexError src sp) pure $ xs V.!? i
            Nothing -> throwError $ TypeError src sp json "Integer"
          json -> throwError $ TypeError src sp json "Integer"
      json -> throwError $ TypeError src (locate t1) json "Object"
  OptionalFieldAccess _ t1 fields -> do
    src <- asks fst
    v1 <- eval t1 `catchError` \_err -> pure J.Null
    fields' <- traverse (either (pure . J.String) eval) fields
    case v1 of
      km@J.Object {} -> maybe (pure J.Null) pure $ evalFieldChain km fields'
      arr@J.Array {} -> maybe (pure J.Null) pure $ evalFieldChain arr fields'
      J.Null -> pure J.Null
      json -> throwError $ TypeError src (locate t1) json "Object"
  Iff sp p t1 t2 -> do
    src <- asks fst
    eval p >>= \case
      J.Bool True -> eval t1
      J.Bool False -> eval t2
      json -> throwError $ TypeError src sp json "Boolean"
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
      (json, J.Bool _) -> throwError $ TypeError src sp json "Boolean"
      (_, json) -> throwError $ TypeError src sp json "Boolean"
  Or sp t1 t2 -> do
    src <- asks fst
    t1' <- eval t1
    t2' <- eval t2
    case (t1', t2') of
      (J.Bool p, J.Bool q) -> pure $ J.Bool $ p || q
      (json, J.Bool _) -> throwError $ TypeError src sp json "Boolean"
      (_, json) -> throwError $ TypeError src sp json "Boolean"
  In sp t1 t2 -> do
    src <- asks fst
    v1 <- eval t1
    v2 <- eval t2
    case (v1, v2) of
      (J.String key, J.Object fields) -> do
        let fields' = fmap fst $ Compat.toList fields
        pure $ J.Bool $ key `elem` fields'
      (json, J.Object _) -> throwError $ TypeError src sp json "String"
      (_, J.Array vals) -> pure $ J.Bool $ v1 `V.elem` vals
      (_, json) -> throwError $ TypeError src sp json "Array"
  Range sp idx binder t1 body -> do
    src <- asks fst
    eval t1 >>= \case
      J.Array arr -> fmap J.Array . flip V.imapM arr $ \i val ->
        let newScope = [(binder, val)] <> [(idxBinder, J.Number $ fromIntegral i) | idxBinder <- maybeToList idx]
         in local (\(_, bndrs) -> (src, Compat.fromList newScope <> bndrs)) (eval body)
      json -> throwError $ TypeError src sp json "Array"
  Function sp fName t1 -> do
    src <- asks fst
    v1 <- eval t1
    case Map.lookup fName funcMap of
      Nothing -> throwError $ NameError src sp fName
      Just f -> case f v1 of
        -- TODO: Bring Custom Errors Into the fold
        Left ee -> throwError $ FunctionError src (locate t1) $ unwrapError ee
        Right va -> pure va
  Defaulting _ t1 t2 -> do
    v1 <- eval t1
    case v1 of
      J.Null -> eval t2
      json -> pure json
  where
    eval = evalWith funcMap
