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
import Data.Bifunctor (second)
import Data.Bits
import Data.Foldable
import Data.Function (on)
import qualified Data.HashMap.Lazy as H
import Data.Int
import Data.List
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, isNothing)
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

-- an evaluator for the core calculus, based on a
-- big-step, call-by-need operational semantics, matching
-- jsonnet specificaton

eval :: Core -> Eval Value
eval = \case
  CLoc sp e -> do
    modify $ updateSpan sp
    eval e
  CLit l -> evalLiteral l
  CVar n -> do
    env <- gets ctx
    v <- liftMaybe (VarNotFound $ T.pack $ name2String n) (M.lookup n env)
    force v
  CFun e -> do
    (bnds, e1) <- unbind e
    evalFun (second unembed <$> unrec bnds) e1
  CApp e1 e2 -> eval e1 >>= \v -> evalApp v e2
  CLet bnd -> mdo
    (r, e1) <- unbind bnd
    bnds <-
      mapM
        ( \(v, Embed e) -> do
            updateEnv bnds
            pure (v, Thunk $ eval e)
        )
        (unrec r)
    updateEnv bnds
    eval e1
  CObj e -> evalObj e
  CArr e -> VArr . V.fromList <$> traverse thunk e
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

thunk :: Core -> Eval Thunk
thunk = mkThunk . eval

evalApp :: Value -> Args Core -> Eval Value
evalApp (VFun f) as = traverse thunk as >>= f
evalApp v@(VClos _) (Positional ps) = foldlM f v ps
  where
    f (VClos c) e = thunk e >>= c
    f v _ = throwTypeMismatch "function" v
evalApp v _ = throwTypeMismatch "function" v

buildParams as bnds = traverse f as
  where
    ns = fst $ unzip bnds
    f (a, b) = case g a of
      Nothing -> throwError $ BadParam $ T.pack a
      Just n -> pure (n, b)
    g a = find ((a ==) . name2String) ns

appDefaults rs e = do
  case findIndex isNothing ds of
    Just x -> throwError $ ParamNotBound (T.pack $ name2String $ ns !! x)
    Nothing -> mdo
      bnds <-
        mapM
          ( \(v, e) -> do
              updateEnv bnds
              pure (v, Thunk $ eval e)
          )
          (zip ns $ catMaybes ds)
      updateEnv bnds
      eval e
  where
    (ns, ds) = unzip rs

evalFun :: [(Var, Maybe Core)] -> Core -> Eval Value
evalFun bnds e = pure $ VFun $ \case
  Positional ps -> do
    case compare (length ns) (length ps) of
      LT -> throwError $ TooManyArgs (length ps)
      EQ -> do
        updateEnv (zip ns ps)
        eval e
      GT -> do
        updateEnv (zip ns ps)
        appDefaults rs e
    where
      rs = drop (length ps) bnds
      (ns, _) = unzip bnds
  Named ps -> do
    (ns, vs) <- unzip <$> buildParams ps bnds
    let rs = filter ((`notElem` ns) . fst) bnds
    updateEnv (zip ns vs)
    appDefaults rs e

evalObj :: [Field Core] -> Eval Value
evalObj xs =
  VObj . H.fromList . map mkKey . filter g <$> traverse f xs
  where
    mkKey (VStr k, v, True) = (Hidden k, v)
    mkKey (VStr k, v, False) = (Visible k, v)
    f Field {..} = do
      a <- eval key
      b <- thunk value
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
evalBinOp In s o = evalBin Std.objectHasAll o s
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
evalComp Eq n1 n2 = VBool <$> Std.equals n1 n2
evalComp Ne n1 n2 = VBool . not <$> Std.equals n1 n2

evalLogical :: LogicalOp -> Value -> Value -> Eval Value
evalLogical LAnd = evalBin (&&)
evalLogical LOr = evalBin (||)

evalBitwise :: BitwiseOp -> Value -> Value -> Eval Value
evalBitwise And = evalBin ((.&.) @Int64)
evalBitwise Or = evalBin ((.|.) @Int64)
evalBitwise Xor = evalBin (xor @Int64)
evalBitwise ShiftL = evalBin (shiftL @Int64)
evalBitwise ShiftR = evalBin (shiftR @Int64)

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

updateEnv :: MonadState EvalState m => [(Var, Thunk)] -> m ()
updateEnv = modify . flip (foldr f)
  where
    f (var, th) st@EvalState {..} = st {ctx = M.insert var th ctx}
