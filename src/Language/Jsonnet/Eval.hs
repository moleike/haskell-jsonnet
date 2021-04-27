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
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty ()
import qualified Language.Jsonnet.Std.Lib as Std
import Language.Jsonnet.Value
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import Control.Lens ((.=), use, locally, view)

-- | an evaluator for the core calculus, based on a
--   big-step, call-by-need operational semantics, matching
--   jsonnet specificaton

eval :: Core -> Eval Value
eval = \case
  CLoc sp e -> locally currentPos (const $ Just sp) (eval e)
  CLit l -> evalLiteral l
  CVar n -> do
    rho <- view env
    v <- liftMaybe (VarNotFound (pretty n)) (M.lookup n rho)
    force v
  CFun f -> VClos f <$> view env
  CPrim op -> pure (VPrim op)
  CApp e es -> evalApp e =<< evalArgs es
  CLet bnd -> evalLetrec bnd
  CObj e -> evalObj e
  CArr e -> VArr . V.fromList <$> traverse thunk e
  CComp (ArrC bnd) cs -> do
    evalArrComp cs bnd
  CComp (ObjC bnd) cs -> do
    evalObjComp cs bnd

thunk :: Core -> Eval Thunk
thunk e =
  view env >>= \rho ->
    mkThunk $ withEnv rho (eval e)

extendEnv' :: [(Name Core, Thunk)] -> Eval a -> Eval a
extendEnv' = extendEnv . M.fromList

evalLetrec (Let bnd) = mdo
  (r, e1) <- unbind bnd
  bnds <-
    mapM
      ( \(v, Embed e) -> do
          th <- mkThunk $ extendEnv' bnds $ eval e
          pure (v, th)
      )
      (unrec r)
  extendEnv' bnds (eval e1)

evalArgs :: Args Core -> Eval [Arg Thunk]
evalArgs = \case
  as@(Args _ Lazy) -> args <$> traverse thunk as
  as@(Args _ Strict) -> args <$> traverse f as
    where
      f = eval >=> pure . mkThunk'

evalApp :: Core -> [Arg Thunk] -> Eval Value
evalApp e vs = withStackFrame e $ do
  eval e >>= \case
    VClos f env -> evalClos env f vs
    VPrim op -> evalPrim op vs
    v@(VFun _) -> foldlM f v vs
      where
        f (VFun g) (Pos v) = g v
        f v _ = throwTypeMismatch "function" v
    v -> throwTypeMismatch "function" v

evalPrim :: Prim -> [Arg Thunk] -> Eval Value
evalPrim (BinOp LAnd) [Pos e1, Pos e2] = evalLogical id e1 e2
evalPrim (BinOp LOr) [Pos e1, Pos e2] = evalLogical not e1 e2
evalPrim (BinOp op) [Pos e1, Pos e2] = liftA2 (,) (force e1) (force e2) >>= uncurry (evalBinOp op)
evalPrim (UnyOp op) [Pos e] = force e >>= evalUnyOp op
evalPrim Cond [Pos c, Pos t, Pos e] = evalCond c t e

withStackFrame :: Core -> Eval a -> Eval a
withStackFrame (CLoc sp (CVar n)) e =
  pushStackFrame (n,  Just sp) e
withStackFrame (CLoc sp _) e =
  pushStackFrame (s2n "anonymous", Just sp) e
withStackFrame (CVar n) e = e
  --pushStackFrame (n, Nothing) e
withStackFrame _ e = e
  --pushStackFrame (s2n "anonymous", Nothing) e

evalCond :: Thunk -> Thunk -> Thunk -> Eval Value
evalCond c e1 e2 = do
  c' <- proj' c
  if c'
    then force e1
    else force e2

evalClos :: Env -> Fun -> [Arg Thunk] -> Eval Value
evalClos rho (Fun f) args = do
  (bnds, e) <- unbind f
  (rs, ps, ns) <- splitArgs args (second unembed <$> unrec bnds)
  withEnv rho $
    extendEnv' ps $
      extendEnv' ns $
        appDefaults rs e

-- all parameter names are bound in default values
appDefaults :: [(Name Core, Core)] -> Core -> Eval Value
appDefaults rs e = mdo
  bnds <-
    mapM
      ( \(n, e) -> do
          th <- mkThunk $ extendEnv' bnds $ eval e
          pure (n, th)
      )
      rs
  extendEnv' bnds (eval e)

-- returns a triple with unapplied binders, positional and named
splitArgs args bnds = do
  named <- getNamed
  pos <- getPos
  unapp <- getUnapp named
  pure (unapp, pos, named)
  where
    (bnds1, bnds2) = splitAt (length ps) bnds
    (ps, ns) = split args
    pNames = fst (unzip bnds)

    getPos = do
      if length ps > length bnds
        then throwE $ TooManyArgs (length bnds)
        else pure $ zip (fst $ unzip bnds1) ps

    -- checks the provided named arguments exist
    getNamed = traverse f ns
      where
        f (a, b) = case g a of
          Nothing -> throwE $ BadParam (pretty a)
          Just n -> pure (n, b)
        g a = find ((a ==) . name2String) pNames

    getUnapp named =
      pure $ filter ((`notElem` ns) . fst) bnds2
        where
          ns = fst (unzip named)

    split [] = ([], [])
    split (Pos p : xs) =
      let (ys, zs) = split xs in (p : ys, zs)
    split (Named n v : xs) =
      let (ys, zs) = split xs in (ys, (n, v) : zs)

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
  rho <- view env
  fs <-
    catMaybes
      <$> mapM
        ( \(KeyValue key value) -> do
            k <- evalKey key
            v <- pure $ TC (M.insert "self" (self fs) rho) <$> value
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
  b <- view env >>= \rho -> pure (TC rho <$> value)
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
          extendEnv' [(n, x)] $ do
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
          extendEnv' [(n, x)] $ do
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
evalUnyOp Err x = (toString >=> throwE . RuntimeError . pretty) x

evalBinOp :: BinOp -> Value -> Value -> Eval Value
evalBinOp Add x@(VStr _) y = inj <$> append x y
evalBinOp Add x y@(VStr _) = inj <$> append x y
evalBinOp Add x@(VArr _) y@(VArr _) = evalBin ((V.++) @Thunk) x y
evalBinOp Add (VObj x) (VObj y) = pure $ VObj (x `mergeWith` y)
evalBinOp Add n1 n2 = evalBin ((+) @Double) n1 n2
evalBinOp Sub n1 n2 = evalBin ((-) @Double) n1 n2
evalBinOp Mul n1 n2 = evalBin ((*) @Double) n1 n2
evalBinOp Div (VNum _) (VNum 0) = throwE DivByZero
evalBinOp Div n1 n2 = evalBin ((/) @Double) n1 n2
evalBinOp Mod (VNum _) (VNum 0) = throwE DivByZero
evalBinOp Mod n1 n2 = evalBin (mod @Int64) n1 n2
evalBinOp Lt e1 e2 = evalBin ((<) @Double) e1 e2
evalBinOp Gt e1 e2 = evalBin ((>) @Double) e1 e2
evalBinOp Le e1 e2 = evalBin ((<=) @Double) e1 e2
evalBinOp Ge e1 e2 = evalBin ((>=) @Double) e1 e2
evalBinOp And e1 e2 = evalBin ((.&.) @Int64) e1 e2
evalBinOp Or e1 e2 = evalBin ((.|.) @Int64) e1 e2
evalBinOp Xor e1 e2 = evalBin (xor @Int64) e1 e2
evalBinOp ShiftL e1 e2 = evalBin (shiftL @Int64) e1 e2
evalBinOp ShiftR e1 e2 = evalBin (shiftR @Int64) e1 e2
evalBinOp Lookup e1 e2 = evalLookup e1 e2
evalBinOp In s o = evalBin (\o s -> Std.objectHasEx o s True) o s

evalLogical f e1 e2 = do
  x <- proj' e1
  if (f x)
    then inj <$> proj' @Bool e2
    else pure (inj x)

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
