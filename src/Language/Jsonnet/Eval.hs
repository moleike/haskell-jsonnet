{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Language.Jsonnet.Eval
  ( eval,
    evalClos,
    mergeWith,
    module Language.Jsonnet.Eval.Monad,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Aeson.Text
import Data.Bifunctor (second)
import Data.Bits
import Data.Foldable
import qualified Data.HashMap.Lazy as H
import Data.Int
import Data.List
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, isNothing)
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Debug.Trace
import Language.Jsonnet.Common hiding (span)
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Manifest
import qualified Language.Jsonnet.Object as O
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty ()
import qualified Language.Jsonnet.Std as Std
import Language.Jsonnet.Value
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

-- an evaluator for the core calculus, based on a
-- big-step, call-by-need operational semantics, matching
-- jsonnet specificaton

eval :: Core -> Eval Value
eval = \case
  CLoc sp e -> do
    --traceShowM (spanBegin sp)
    updateSpan (Just sp) >> eval e
  CLit l -> evalLiteral l
  CVar n -> do
    env <- asks ctx
    --sp <- gets currentPos
    --traceShowM (spanBegin <$> sp)
    v <- liftMaybe (VarNotFound (pretty n)) (M.lookup n env)
    force v
  CFun f -> VClos f <$> ask
  CApp e es -> evalApp e =<< evalArgs es
  cc@(CLet (Let bnd)) -> mdo
    (r, e1) <- unbind bnd
    bnds <-
      mapM
        ( \(v, Embed e) -> do
            th <- mkThunk $ extendCtx' bnds $ eval e
            pure (v, th)
        )
        (unrec r)

    --traceShowM "applying the body of the local binding"
    --traceShowM e1
    extendCtx' bnds (eval e1)
  CObj e -> evalObj e
  CArr e -> VArr . V.fromList <$> traverse thunk e
  CBinOp (Logical op) e1 e2 -> do
    e1' <- thunk e1
    e2' <- thunk e2
    evalLogical op e1' e2'
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
  CIfElse c e1 e2 -> do
    eval c >>= \case
      VBool b ->
        if b
          then eval e1
          else eval e2
      v -> throwTypeMismatch "bool" v
  CErr e ->
    ( eval
        >=> toString
        >=> throwE . RuntimeError . pretty
    )
      e
  CComp (ArrC bnd) cs -> do
    evalArrComp cs bnd
  CComp (ObjC bnd) cs -> do
    evalObjComp cs bnd

thunk :: Core -> Eval Thunk
thunk e =
  ask >>= \rho ->
    mkThunk $ withCtx (ctx rho) (eval e)

extendCtx' :: [(Name Core, Thunk)] -> Eval a -> Eval a
extendCtx' = extendCtx . M.fromList

evalArgs :: Args Core -> Eval [Arg Thunk]
evalArgs = \case
  as@(Args _ Lazy) -> args <$> traverse thunk as
  as@(Args _ Strict) -> args <$> traverse f as
    where
      f = eval >=> pure . mkThunk'

evalApp :: Core -> [Arg Thunk] -> Eval Value
evalApp e vs = withStackFrame e $ do
  eval e >>= \case
    VClos f Env {..} -> evalClos ctx f vs
    v@(VFun _) -> foldlM f v vs
      where
        f (VFun g) (Pos v) = g v
        f v _ = throwTypeMismatch "function" v
    v -> throwTypeMismatch "function" v

withStackFrame :: Core -> Eval a -> Eval a
withStackFrame (CLoc sp (CVar n)) e =
  pushScope n (pushSpan (Just sp) e)
withStackFrame (CLoc sp _) e =
  pushScope (s2n "anonymous") (pushSpan (Just sp) e)
withStackFrame (CVar n) e =
  pushScope n (pushSpan Nothing e)
withStackFrame _ e =
  pushScope (s2n "anonymous") (pushSpan Nothing e)

evalClos :: Ctx -> Fun -> [Arg Thunk] -> Eval Value
evalClos rho (Fun f) vs = do
  (bnds, e) <- unbind f
  let xs = second unembed <$> unrec bnds
  withCtx rho (evalFun xs e vs)

appDefaults :: [(Name Core, Maybe Core)] -> Core -> Eval Value
appDefaults rs e = do
  case findIndex isNothing ds of
    Just x -> throwE $ ParamNotBound (pretty $ ns !! x)
    Nothing -> mdo
      bnds <-
        mapM
          ( \(v, e) -> do
              th <- mkThunk $ extendCtx' bnds $ eval e
              pure (v, th)
          )
          (zip ns $ catMaybes ds)
      extendCtx' bnds (eval e)
  where
    (ns, ds) = unzip rs

evalFun bnds e args = do
  if length ps > length bnds
    then throwE $ TooManyArgs (length bnds)
    else extendCtx' (zip names ps') $ evalNamedArgs ns bnds'
  where
    isPos = \case
      Pos _ -> True
      _ -> False
    (ps, ns) = span isPos args
    ps' = fmap (\(Pos a) -> a) ps
    (names, _) = unzip bnds
    bnds' = drop (length ps) bnds
    evalNamedArgs ns bnds = do
      ns' <- forM ns $ \case
        Named n v -> pure (n, v)
      (names, vs) <- unzip <$> buildParams ns' bnds
      let rs = filter ((`notElem` names) . fst) bnds
      extendCtx' (zip names vs) (appDefaults rs e)
      where
        buildParams as bnds = traverse f as
          where
            ns = fst $ unzip bnds
            f (a, b) = case g a of
              Nothing -> throwE $ BadParam (pretty a)
              Just n -> pure (n, b)
            g a = find ((a ==) . name2String) ns

-- | right-biased union of two objects, i.e. '{x : 1} + {x : 2} == {x : 2}'
mergeWith :: Object -> Object -> Object
mergeWith xs ys =
  let f a b
        | hidden a && visible b = a
        | otherwise = b
      g name xs = fmap $
        \case
          TC rho e -> TC (M.insert name (mkThunk' $ VObj xs) rho) e
          v@(TV {}) -> v
      h name xs = fmap $
        \case
          TC rho e -> TC (insert' name (mkThunk' $ VObj xs) rho) e
          v@(TV {}) -> v
      xs' = H.map (g "self" zs') xs
      ys' = H.map (g "self" zs' . h "super" xs') ys
      zs' = H.unionWith f xs' ys'
      insert' = M.insertWith (const)
   in zs'

evalObj :: [KeyValue Core] -> Eval Value
evalObj xs = mdo
  env <- asks ctx
  fs <-
    catMaybes
      <$> mapM
        ( \(KeyValue key value) -> do
            k <- evalKey key
            v <- pure $ TC (M.insert "self" (self fs) env) <$> value
            case k of
              Just k -> pure $ Just (k, v)
              _ -> pure Nothing
        )
        xs
  pure $ VObj $ H.fromList $ fs
  where
    self = mkThunk' . VObj . H.fromList

evalKey :: Core -> Eval (Maybe Text)
evalKey key =
  eval key >>= \case
    VStr k -> pure $ Just k
    VNull -> pure Nothing
    v -> throwInvalidKey v

evalKeyValue :: KeyValue Core -> Eval (Maybe (Text, Hideable Thunk))
evalKeyValue (KeyValue key value) = do
  a <- evalKey key
  b <- asks ctx >>= \rho -> pure (TC rho <$> value)
  pure $ (,b) <$> a

evalArrComp ::
  Core ->
  Bind (Name Core) (Core, Maybe Core) ->
  Eval Value
evalArrComp cs bnd = do
  xs <- comp
  inj' flattenArrays $ VArr $ V.mapMaybe id xs
  where
    comp =
      eval cs >>= \case
        VArr xs -> forM xs $ \x -> do
          (n, (e, cond)) <- unbind bnd
          extendCtx' [(n, x)] $ do
            b <- f cond
            if b
              then Just <$> thunk e
              else pure Nothing
        v -> throwTypeMismatch "array" v
      where
        f Nothing = pure True
        f (Just c) = do
          vb <- eval c
          proj vb

evalObjComp ::
  Core ->
  Bind (Name Core) (KeyValue Core, Maybe Core) ->
  Eval Value
evalObjComp cs bnd = do
  xs <- comp
  pure $ VObj $ H.fromList $ catMaybes $ V.toList xs
  where
    comp =
      eval cs >>= \case
        VArr xs -> forM xs $ \x -> do
          (n, (e, cond)) <- unbind bnd
          extendCtx' [(n, x)] $ do
            b <- f cond
            if b
              then evalKeyValue e
              else pure Nothing
        v -> throwTypeMismatch "array" v
    f Nothing = pure True
    f (Just c) = do
      vb <- eval c
      proj vb

evalUnyOp :: UnyOp -> Value -> Eval Value
evalUnyOp Compl x = inj <$> fmap (complement @Int64) (proj x)
evalUnyOp LNot x = inj <$> fmap not (proj x)
evalUnyOp Minus x = inj <$> fmap (negate @Double) (proj x)
evalUnyOp Plus x = inj <$> fmap (id @Double) (proj x)

evalBinOp :: BinOp -> Value -> Value -> Eval Value
evalBinOp In s o = evalBin (\o s -> Std.objectHasEx o s True) o s
evalBinOp (Arith Add) x@(VStr _) y = inj <$> append x y
evalBinOp (Arith Add) x y@(VStr _) = inj <$> append x y
evalBinOp (Arith Add) x@(VArr _) y@(VArr _) = evalBin ((V.++) @Thunk) x y
evalBinOp (Arith Add) (VObj x) (VObj y) = pure $ VObj (x `mergeWith` y)
evalBinOp (Arith op) x y = evalArith op x y
evalBinOp (Comp op) x y = evalComp op x y
evalBinOp (Bitwise op) x y = evalBitwise op x y

evalArith :: ArithOp -> Value -> Value -> Eval Value
evalArith Add n1 n2 = evalBin ((+) @Double) n1 n2
evalArith Sub n1 n2 = evalBin ((-) @Double) n1 n2
evalArith Mul n1 n2 = evalBin ((*) @Double) n1 n2
evalArith Div (VNum _) (VNum 0) = throwE DivByZero
evalArith Div n1 n2 = evalBin ((/) @Double) n1 n2
evalArith Mod (VNum _) (VNum 0) = throwE DivByZero
evalArith Mod n1 n2 = evalBin (mod @Int64) n1 n2

evalComp :: CompOp -> Value -> Value -> Eval Value
evalComp Lt n1 n2 = evalBin ((<) @Double) n1 n2
evalComp Gt n1 n2 = evalBin ((>) @Double) n1 n2
evalComp Le n1 n2 = evalBin ((<=) @Double) n1 n2
evalComp Ge n1 n2 = evalBin ((>=) @Double) n1 n2

evalLogical :: LogicalOp -> Thunk -> Thunk -> Eval Value
evalLogical LAnd e1 e2 = do
  force e1 >>= \case
    VBool True -> force e2 >>= \x -> inj <$> fmap (id @Bool) (proj x)
    VBool False -> pure (VBool False)
    v -> throwTypeMismatch "boolean" v
evalLogical LOr e1 e2 = do
  force e1 >>= \case
    VBool False -> force e2 >>= \x -> inj <$> fmap (id @Bool) (proj x)
    VBool True -> pure (VBool True)
    v -> throwTypeMismatch "boolean" v

evalBitwise :: BitwiseOp -> Value -> Value -> Eval Value
evalBitwise And = evalBin ((.&.) @Int64)
evalBitwise Or = evalBin ((.|.) @Int64)
evalBitwise Xor = evalBin (xor @Int64)
evalBitwise ShiftL = evalBin (shiftL @Int64)
evalBitwise ShiftR = evalBin (shiftR @Int64)

evalLookup :: Value -> Value -> Eval Value
evalLookup (VArr a) (VNum i)
  | isInteger i =
    liftMaybe (IndexOutOfBounds i) ((a !?) =<< toBoundedInteger i) >>= force
evalLookup (VArr _) _ =
  throwE (InvalidIndex $ "array index was not integer")
evalLookup (VObj o) (VStr s) =
  liftMaybe (NoSuchKey (pretty s)) (H.lookup s o)
    >>= \(Hideable v _) -> force v
evalLookup (VStr s) (VNum i) | isInteger i = do
  liftMaybe (IndexOutOfBounds i) (f =<< bounded)
  where
    f = pure . VStr . T.singleton . T.index s
    bounded =
      toBoundedInteger i >>= \i' ->
        if T.length s - 1 < i' && i' < 0
          then Nothing
          else Just i'
evalLookup (VStr _) _ =
  throwE (InvalidIndex $ "string index was not integer")
evalLookup v _ = throwTypeMismatch "array/object/string" v

evalLiteral :: Literal -> Eval Value
evalLiteral = \case
  Null -> pure VNull
  Bool b -> pure $ VBool b
  String s -> pure $ VStr s
  Number n -> pure $ VNum n

evalBin ::
  (HasValue a, HasValue b, HasValue c) =>
  (a -> b -> c) ->
  Value ->
  Value ->
  Eval Value
evalBin = inj''

append :: Value -> Value -> Eval Text
append v1 v2 = T.append <$> toString v1 <*> toString v2

throwInvalidKey :: Value -> Eval a
throwInvalidKey = throwE . InvalidKey . pretty . valueType

updateSpan :: Maybe SrcSpan -> Eval ()
updateSpan sp = modify $ \st -> st {currentPos = sp}

toString :: Value -> Eval Text
toString (VStr s) = pure s
toString v = toStrict . encodeToLazyText <$> manifest v

flattenArrays :: Vector (Vector Thunk) -> Vector Thunk
flattenArrays = join

liftMaybe :: EvalError -> Maybe a -> Eval a
liftMaybe e =
  \case
    Nothing -> throwE e
    Just a -> pure a
