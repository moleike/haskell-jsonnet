{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Language.Jsonnet.Eval
  ( eval,
    module Language.Jsonnet.Eval.Monad,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Bits
import qualified Data.HashMap.Lazy as H
import Data.Int
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector ((!?))
import qualified Data.Vector as V
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty ()
import qualified Language.Jsonnet.Std as Std
import Language.Jsonnet.Value
import Unbound.Generics.LocallyNameless

eval :: Core -> Eval Value
eval = \case
  CLoc sp e -> do
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
    VBool b ->
      if b
        then eval e1
        else eval e2
    v -> throwTypeMismatch "bool" v
  CErr e ->
    ( eval
        >=> Std.toString
        >=> throwError . RuntimeError
    )
      e

evalObj :: Object Core -> Eval Value
evalObj (Object o) =
  VObj . H.fromList . map mkKey . filter g <$> traverse f o
  where
    mkKey (VStr k, v, hidden) =
      if hidden
        then (Hidden k, v)
        else (Visible k, v)
    f KeyValue {..} = do
      a <- eval key
      b <- mkThunk (eval value)
      case a of
        VStr _ -> pure (a, b, hidden)
        VNull -> pure (a, b, hidden)
        v -> throwInvalidKey v
    g (k, _, _) = case k of
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
evalArith Div (VNum _) (VNum 0) = throwError DivByZero
evalArith Div n1 n2 = evalBin ((/) @Double) n1 n2
evalArith Mod (VNum _) (VNum 0) = throwError DivByZero
evalArith Mod n1 n2 = evalBin (mod @Int64) n1 n2

evalComp :: CompOp -> Value -> Value -> Eval Value
evalComp Lt n1 n2 = evalBin ((<) @Double) n1 n2
evalComp Gt n1 n2 = evalBin ((>) @Double) n1 n2
evalComp Le n1 n2 = evalBin ((<=) @Double) n1 n2
evalComp Ge n1 n2 = evalBin ((>=) @Double) n1 n2
evalComp Eq n1 n2 = VBool <$> n1 `Std.equals` n2
evalComp Ne n1 n2 = VBool . not <$> n1 `Std.equals` n2

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
  liftMaybe (IndexOutOfBounds $ round i) (a !? round i) >>= force
evalLookup (VObj o) (VStr s) =
  liftMaybe (NoSuchKey s) (H.lookup (Visible s) o <|> H.lookup (Hidden s) o) >>= force
evalLookup (VStr s) (VNum i)
  | i' < 0 = throwError (IndexOutOfBounds i')
  | T.length s - 1 < i' = throwError (IndexOutOfBounds i')
  | otherwise = pure $ VStr $ T.singleton $ s `T.index` i'
  where
    i' = round i
evalLookup v _ = throwTypeMismatch "array/object/string" v

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

evalBin ::
  (HasValue a, HasValue b, HasValue c) =>
  (a -> b -> c) ->
  Value ->
  Value ->
  Eval Value
evalBin = inj''

append :: Value -> Value -> Eval Text
append v1 v2 = T.append <$> Std.toString v1 <*> Std.toString v2

throwInvalidKey :: MonadError EvalError m => Value -> m a
throwInvalidKey = throwError . InvalidKey . valueType

updateSpan :: SrcSpan -> EvalState -> EvalState
updateSpan sp st = st {curSpan = Just sp}

updateCtx :: (Var, Thunk) -> EvalState -> EvalState
updateCtx (var, th) st@EvalState {..} = st {ctx = M.insert var th ctx}
