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
import Kriti.Parser.Spans as Spans
import Kriti.Parser.Token
import Prettyprinter as P
import qualified Network.URI as URI
import qualified Data.ByteString.UTF8 as B

data EvalError
  = InvalidPath B.ByteString Span (V.Vector Accessor)
  | TypeError B.ByteString Span T.Text 
  | RangeError B.ByteString Span
  deriving Show

instance Pretty EvalError where
   pretty = \case
     InvalidPath src term _ -> mkPretty src "Invalid path lookup" term
     TypeError src term msg -> mkPretty src msg term
     RangeError src term -> mkPretty src "Index out of range" term
     where
       mkPretty source msg term =
         let AlexSourcePos { line = startLine, col = startCol } = start $ locate term
             AlexSourcePos { col = endCol } = end $ locate term
             sourceLine = B.lines source !! startLine
         in vsep [ "Runtime Error:" ,
                   indent 2 $ pretty (msg :: T.Text),
                   indent (startLine + 1) "|",
                   pretty startLine <+>  "|" <+> pretty (TE.decodeUtf8 sourceLine),
                   indent (startLine + 1) $ "|" <> indent (startCol) (pretty $ replicate (endCol - startCol) '^')
                 ]

instance SerializeError EvalError where
  serialize :: EvalError -> SerializedError 
  serialize (InvalidPath _ term path) = SerializedError  {_code = InvalidPathCode, _message = "\"" <> renderVect path <> "\"", _span = locate term}
  serialize (TypeError _ term msg) = SerializedError  {_code = TypeErrorCode, _message = msg, _span = locate term}
  serialize (RangeError _ term) = SerializedError  {_code = RangeErrorCode, _message = "Can only range over an array", _span = locate term}

type Ctxt = (B.ByteString, Compat.Object J.Value)

getSourcePos :: EvalError -> Span
getSourcePos (InvalidPath _ pos _) = locate pos
getSourcePos (TypeError _ term _) = locate term
getSourcePos (RangeError _ pos) = locate pos

evalPath :: Span -> J.Value -> V.Vector (Accessor) -> ExceptT EvalError (Reader Ctxt) J.Value
evalPath sp ctx path = do
  src <- asks fst
  let step :: Monad m => J.Value -> Accessor -> ExceptT EvalError m J.Value
      step (J.Object o) (Obj _ k _) = maybe (throwError $ InvalidPath src sp path) pure $ Compat.lookup k o
      step (J.Array xs) (Arr _ i) = maybe (throwError $ InvalidPath src sp path) pure $ xs V.!? i
      step _ (Obj _ _ _) = throwError $ TypeError src sp "Expected object"
      step _ (Arr _ _) = throwError $ TypeError src sp "Expected array"
   in foldlM step ctx path

isString :: J.Value -> Bool
isString J.String {} = True
isString _ = False

runEval :: B.ByteString -> ValueExt -> [(T.Text, J.Value)] -> Either EvalError J.Value
runEval src template source =
  let ctx = Compat.fromList source
   in runReader (runExceptT (eval template)) (src, ctx)

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
  StringTem sp ts -> do
    src <- asks fst
    vals <- traverse eval ts
    str <-
      vals & flip foldlM mempty \acc -> \case
        J.String val' -> pure $ acc <> val'
        J.Number i -> pure $ acc <> TE.decodeUtf8 (BL.toStrict $ J.encode i)
        J.Bool p -> pure $ acc <> TE.decodeUtf8 (BL.toStrict $ J.encode p)
        t -> throwError $ TypeError src sp $ "Cannot interpolate type: '" <> typoOfJSON t <> "'."
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
  Member sp t ts -> do
    src <- asks fst
    ts' <- eval ts
    case ts' of
      J.Array xs -> do
        t' <- eval t
        pure $ J.Bool $ t' `V.elem` xs
      _ -> throwError $ TypeError src sp $ T.pack $ "'" <> show ts' <> "' is not an array."
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
