{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Jsonnet.Eval where

import Control.Applicative
import Control.Arrow
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Bits
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.IORef
import Data.Int
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector ((!?), Vector)
import qualified Data.Vector as V
import Debug.Trace
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.JSON
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty ()
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Unbound.Generics.LocallyNameless

type Eval = FreshMT (ExceptT EvalError (StateT EvalState IO))

type MonadEval m =
  ( Fresh m,
    MonadError EvalError m,
    MonadState EvalState m,
    MonadIO m
  )

data EvalState = EvalState
  { ctx :: Map Var Thunk,
    curSpan :: Maybe SrcSpan
  }

emptyState :: EvalState
emptyState = EvalState M.empty Nothing

runEval ::
  EvalState ->
  Eval a ->
  ExceptT (EvalError, EvalState) IO a
runEval st comp = ExceptT $ (`evalStateT` st) $ do
  res <- runExceptT $ runFreshMT comp
  st' <- get
  pure $ left (,st') res

newtype Thunk = Thunk {force :: Eval Value}

mkThunk :: MonadEval m => Eval Value -> m Thunk
mkThunk ev = do
  ref <- liftIO $ newIORef Nothing
  pure $ Thunk $
    liftIO (readIORef ref) >>= \case
      Nothing -> do
        v <- ev
        liftIO $ writeIORef ref $ Just v
        pure v
      Just v -> pure v

-- jsonnet value
data Value
  = VNull
  | VBool !Bool
  | VNum !Double
  | VStr !Text
  | VArr !(Vector Thunk)
  | VObj !(HashMap Text Thunk)
  | VClos !(Thunk -> Eval Value)

proj' :: HasValue a => Thunk -> Eval a
proj' = force >=> proj

class HasValue a where
  proj :: Value -> Eval a
  inj :: a -> Value

instance {-# OVERLAPS #-} HasValue Value where
  proj = pure
  inj = id

instance HasValue Bool where
  proj (VBool n) = pure n
  proj v = throwTypeMismatch "bool" v
  inj = VBool

instance HasValue Text where
  proj (VStr s) = pure s
  proj v = throwTypeMismatch "string" v
  inj = VStr

instance HasValue Double where
  proj (VNum n) = pure n
  proj v = throwTypeMismatch "number" v
  inj = VNum

instance {-# OVERLAPS #-} Integral a => HasValue a where
  proj (VNum n) = pure (round n)
  proj v = throwTypeMismatch "number" v
  inj = VNum . fromIntegral

instance HasValue a => HasValue (Maybe a) where
  proj VNull = pure Nothing
  proj a = Just <$> proj a
  inj Nothing = VNull
  inj (Just a) = inj a

instance HasValue a => HasValue (Vector a) where
  proj (VArr as) = traverse proj' as
  proj v = throwTypeMismatch "array" v
  inj as = VArr $ (Thunk . pure . inj) <$> as

instance {-# OVERLAPPABLE #-} HasValue a => HasValue [a] where
  proj = fmap V.toList . proj
  inj = inj . V.fromList

instance {-# OVERLAPPING #-} HasValue [Char] where
  proj (VStr s) = pure $ T.unpack s
  proj v = throwTypeMismatch "string" v
  inj = VStr . T.pack

instance HasValue a => HasValue (HashMap Text a) where
  proj (VObj o) = traverse proj' o
  proj v = throwTypeMismatch "object" v
  inj o = VObj $ (Thunk . pure . inj) <$> o

instance {-# OVERLAPS #-} (HasValue a, HasValue b) => HasValue (a -> b) where
  proj v = throwTypeMismatch "impossible" v
  inj f = VClos $ \x -> force x >>= fmap (inj . f) . proj

instance {-# OVERLAPS #-} (HasValue a, HasValue b, HasValue c) => HasValue (a -> b -> c) where
  proj v = throwTypeMismatch "impossible" v
  inj f = inj $ \x -> inj (f x)

instance {-# OVERLAPS #-} (HasValue a, HasValue b) => HasValue (a -> Eval b) where
  proj (VClos f) = pure $ \x -> do
    r <- f (Thunk $ pure $ inj x)
    proj r
  proj v = throwTypeMismatch "function" v
  inj f = VClos $ \v -> proj' v >>= fmap inj . f

instance {-# OVERLAPS #-} (HasValue a, HasValue b, HasValue c) => HasValue (a -> b -> Eval c) where
  proj (VClos f) = pure $ \x y -> do
    VClos g <- f (Thunk $ pure $ inj x)
    r <- g (Thunk $ pure $ inj y)
    proj r
  proj v = throwTypeMismatch "function" v
  inj f = inj $ \x -> inj (f x)

updateSpan :: SrcSpan -> EvalState -> EvalState
updateSpan sp st = st {curSpan = Just sp}

updateCtx :: (Var, Thunk) -> EvalState -> EvalState
updateCtx (var, th) st@EvalState {..} = st {ctx = M.insert var th ctx}

eval :: Core -> Eval Value
eval = \case
  CAnno sp e -> do
    modify $ updateSpan sp
    eval e
  CLit l -> evalLiteral l
  CVar n -> do
    env <- gets ctx
    v <- liftMaybe (VarNotFound $ T.pack $ show n) (M.lookup n env)
    force v
  CLam bnd -> do
    (n, e) <- unbind bnd
    pure $ VClos $ \th -> do
      modify $ updateCtx (n, th)
      eval e
  CApp e1 e2 -> do
    eval e1 >>= \case
      VClos c -> mkThunk (eval e2) >>= c
      v -> throwTypeMismatch "function" v
  CLet bnd -> mdo
    (r, e1) <- unbind bnd
    bnds <-
      mapM
        ( \(v, Embed e) -> do
            modify $ flip (foldr updateCtx) bnds
            pure (v, Thunk $ eval e)
        )
        (unrec r)
    modify $ flip (foldr updateCtx) bnds
    eval e1
  CObj bnd -> mdo
    (self, e) <- unbind bnd
    -- tying the knot
    th <- mkThunk $ do
      modify $ updateCtx (self, th)
      evalObj e
    modify $ updateCtx (self, th)
    force th
  CArr e -> VArr . V.fromList <$> traverse (mkThunk . eval) e
  CBinOp op e1 e2 -> do
    e1' <- eval e1
    e2' <- eval e2
    evalBinOp op e1' e2'
  CUnyOp op e -> do
    e' <- eval e
    evalUnyOp op e'
  CLookup e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    evalLookup v1 v2
  CIfElse c e1 e2 -> eval c >>= \case
    VBool b -> if b then eval e1 else eval e2
    v -> throwTypeMismatch "bool" v
  CErr e ->
    (eval >=> toString) e
      >>= throwError . RuntimeError
  CAssert c m e -> eval c >>= \case
    VBool b -> if b
      then eval e
      else throwError (AssertionFailed $ pretty m)
    v -> throwTypeMismatch "bool" v

evalObj :: Object Core -> Eval Value
evalObj (Object o) =
  VObj . H.fromList . map (\(VStr k, v) -> (k, v)) . filter g <$> traverse f o
  where
    f KeyValue {..} = do
      a <- eval key
      b <- mkThunk (eval value)
      case a of
        VStr _ -> pure (a, b)
        VNull -> pure (a, b)
        v -> throwInvalidKey v
    g (k, _) = case k of
      VNull -> False
      _ -> True

evalUnyOp :: UnyOp -> Value -> Eval Value
evalUnyOp Compl x = inj <$> fmap (complement @Int64) (proj x)
evalUnyOp LNot x = inj <$> fmap not (proj x)
evalUnyOp Minus x = inj <$> fmap (negate @Double) (proj x)
evalUnyOp Plus x = inj <$> fmap (id @Double) (proj x)

evalBinOp :: BinOp -> Value -> Value -> Eval Value
evalBinOp (Arith Add) x@(VStr _) y = inj <$> append x y
evalBinOp (Arith Add) x y@(VStr _) = inj <$> append x y
evalBinOp (Arith op) x y = evalArith op x y
evalBinOp (Comp op) x y = evalComp op x y
evalBinOp (Bitwise op) x y = evalBitwise op x y
evalBinOp (Logical op) x y = evalLogical op x y

evalArith :: ArithOp -> Value -> Value -> Eval Value
evalArith Add n1 n2 = evalBin ((+) @Double) n1 n2
evalArith Sub n1 n2 = evalBin ((-) @Double) n1 n2
evalArith Mul n1 n2 = evalBin ((*) @Double) n1 n2
evalArith Div (VNum _) (VNum 0) = throwError (DivByZero)
evalArith Div n1 n2 = evalBin ((/) @Double) n1 n2
evalArith Mod n1 n2 = evalBin (mod @Int64) n1 n2

evalComp :: CompOp -> Value -> Value -> Eval Value
evalComp Lt n1 n2 = evalBin ((<) @Double) n1 n2
evalComp Gt n1 n2 = evalBin ((>) @Double) n1 n2
evalComp Le n1 n2 = evalBin ((<=) @Double) n1 n2
evalComp Ge n1 n2 = evalBin ((>=) @Double) n1 n2
evalComp Eq n1 n2 = VBool <$> n1 `equalTo` n2
evalComp Ne n1 n2 = (VBool . not) <$> n1 `equalTo` n2

evalLogical :: LogicalOp -> Value -> Value -> Eval Value
evalLogical LAnd n1 n2 = evalBin (&&) n1 n2
evalLogical LOr n1 n2 = evalBin (||) n1 n2

evalBitwise :: BitwiseOp -> Value -> Value -> Eval Value
evalBitwise And n1 n2 = evalBin ((.&.) @Int64) n1 n2
evalBitwise Or n1 n2 = evalBin ((.|.) @Int64) n1 n2
evalBitwise Xor n1 n2 = evalBin (xor @Int64) n1 n2
evalBitwise ShiftL n1 n2 = evalBin (shiftL @Int64) n1 n2
evalBitwise ShiftR n1 n2 = evalBin (shiftR @Int64) n1 n2

evalLookup :: Value -> Value -> Eval Value
evalLookup (VArr a) (VNum i) =
  liftMaybe (IndexOutOfBounds (round i)) (a !? round i) >>= force
evalLookup (VObj o) (VStr s) =
  liftMaybe (NoSuchKey s) (H.lookup s o) >>= force
evalLookup v _ = throwTypeMismatch "array/object" v

evalLiteral :: Literal -> Eval Value
evalLiteral = \case
  Null -> pure VNull
  Bool b -> pure $ VBool b
  String b -> pure $ VStr b
  Number b -> pure $ VNum b

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e =
  \case
    Nothing -> throwError e
    Just a -> pure a

equalTo :: Value -> Value -> Eval Bool
equalTo a b = (==) <$> manifest a <*> manifest b

toString :: Value -> Eval Text
toString (VStr s) = pure s
toString v = (T.pack . show . pretty) <$> manifest v

append :: Value -> Value -> Eval Text
append v1 v2 = T.append <$> toString v1 <*> toString v2

valueType :: Value -> Text
valueType =
  \case
    VNull -> "null"
    VBool _ -> "boolean"
    VNum _ -> "number"
    VStr _ -> "string"
    VArr _ -> "array"
    VObj _ -> "object"
    VClos _ -> "function"

throwTypeMismatch :: MonadError EvalError m => Text -> Value -> m a
throwTypeMismatch expected =
  throwError . TypeMismatch expected
    . valueType

throwInvalidKey :: MonadError EvalError m => Value -> m a
throwInvalidKey = throwError . InvalidKey . valueType

evalBin ::
  (HasValue a, HasValue b, HasValue c) =>
  (a -> b -> c) ->
  Value ->
  Value ->
  Eval Value
evalBin f v1 v2 = inj <$> liftA2 f (proj v1) (proj v2)

manifest :: Value -> Eval JSON
manifest =
  \case
    VNull -> pure JNull
    VBool b -> pure $ JBool b
    VNum n -> pure $ JNum n
    VStr s -> pure $ JStr s
    VArr a -> JArr <$> traverse (force >=> manifest) a
    VObj o -> JObj <$> traverse (force >=> manifest) o
    VClos _ -> throwError (ManifestError $ NotAJsonValue "function")
