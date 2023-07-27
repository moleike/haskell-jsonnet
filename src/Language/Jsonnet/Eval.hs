{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module                  : Language.Jsonnet.Eval
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Eval where

import Control.Applicative
import Control.Lens (locally, view)
import Control.Monad ((>=>), join, forM, (<=<))
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (second)
import qualified Data.Bits as Bits
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.IORef
import Data.Int (Int64)
import qualified Data.List as L (sort)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Traversable (for)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Debug.Trace
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Pretty ()
import Language.Jsonnet.Value
import Prettyprinter hiding (equals)
import Unbound.Generics.LocallyNameless
import Prelude hiding (length)
import qualified Prelude as P (length)

rnf :: Core -> Eval JSON.Value
rnf = whnf >=> manifest

whnfV :: Value -> Eval Value
whnfV (VIndir loc) = whnfIndir loc >>= whnfV
whnfV (VThunk c e) = withEnv e (whnf c)
whnfV v = pure v

whnf :: Core -> Eval Value
whnf (CVar n) = lookupVar n
whnf (CLoc sp c) = locally currentPos (const $ Just sp) (whnf c)
whnf (CLit l) = pure (whnfLiteral l)
whnf (CObj bnd) = whnfObj bnd
whnf (CArr cs) = VArr . V.fromList <$> mapM mkValue cs
whnf (CLet bnd) = whnfLetrec bnd
whnf (CPrim p) = pure (VPrim p)
whnf (CApp e es) = whnfApp e es
whnf (CLam f) = VClos f <$> view ctx
whnf (CComp comp e) = whnfComp comp e

mkValue :: Core -> Eval Value
mkValue c@(CLit _) = whnf c
mkValue c@(CLam _) = whnf c
mkValue c@(CPrim _) = whnf c
mkValue c = mkThunk c >>= mkIndirV

lookupVar :: Name Core -> Eval Value
lookupVar n = do
  rho <- view ctx
  v <- liftMaybe (VarNotFound (n2s n)) (M.lookup n rho)
  whnfV v

whnfLiteral :: Literal -> Value
whnfLiteral = \case
  Null -> VNull
  Bool b -> VBool b
  String s -> VStr s
  Number n -> VNum n

whnfArgs :: Args Core -> Eval [Arg Value]
whnfArgs = \case
  as@(Args _ Strict) -> args <$> mapM whnf as
  as@(Args _ Lazy) -> args <$> mapM mkValue as

whnfApp :: Core -> Args Core -> Eval Value
whnfApp e es = withStackFrame e $ do
  vs <- whnfArgs es
  whnf e >>= whnfV >>= \case
    VClos f env -> whnfClos env f vs
    VPrim op -> whnfPrim op vs
    v@(VFun _) -> foldlM f v vs
      where
        f (VFun g) (Pos v) = g v
        f v _ = throwTypeMismatch "function" v
    v -> throwTypeMismatch "function" v

withStackFrame :: Core -> Eval a -> Eval a
withStackFrame (CLoc sp (CVar n)) e =
  pushStackFrame (n, Just sp) e
withStackFrame (CLoc sp _) e =
  pushStackFrame (s2n "anonymous", Just sp) e
withStackFrame (CVar _) e = e
--pushStackFrame (n, Nothing) e
withStackFrame _ e = e

--pushStackFrame (s2n "anonymous", Nothing) e

whnfClos :: Env -> Lam -> [Arg Value] -> Eval Value
whnfClos rho f args = do
  (bnds, e) <- unbind f
  (rs, ps, ns) <- splitArgs args (second unembed <$> unrec bnds)
  withEnv rho $
    extendEnv (M.fromList ps) $
      extendEnv (M.fromList ns) $
        appDefaults rs e

-- all parameter names are bound in default values
appDefaults :: [(Name Core, Core)] -> Core -> Eval Value
appDefaults rs e = mdo
  bnds <-
    M.fromList
      <$> mapM
        ( \(n, e) -> do
            th <- extendEnv bnds (mkValue e)
            pure (n, th)
        )
        rs
  extendEnv bnds (whnf e)

-- returns a triple with unapplied binders, positional and named
splitArgs args bnds = do
  named <- getNamed
  pos <- getPos
  unapp <- getUnapp named
  pure (unapp, pos, named)
  where
    (bnds1, bnds2) = splitAt (length ps) bnds
    (ps, ns) = split args
    pNames = map fst bnds

    getPos =
      if length ps > length bnds
        then throwE $ TooManyArgs (length bnds)
        else pure $ zip (map fst bnds1) ps

    -- checks the provided named arguments exist
    getNamed = traverse f ns
      where
        f (a, b) = case g a of
          Nothing -> throwE $ BadParam (T.pack a)
          Just n -> pure (n, b)
        g a = find ((a ==) . name2String) pNames

    getUnapp named =
      pure $ filter ((`notElem` ns) . fst) bnds2
      where
        ns = map fst named

    split [] = ([], [])
    split (Pos p : xs) =
      let (ys, zs) = split xs in (p : ys, zs)
    split (Named n v : xs) =
      let (ys, zs) = split xs in (ys, (n, v) : zs)

whnfPrim :: Prim -> [Arg Value] -> Eval Value
whnfPrim (UnyOp op) [Pos e] = whnfV e >>= whnfUnyOp op
whnfPrim (BinOp LAnd) [Pos e1, Pos e2] = whnfLogical id e1 e2
whnfPrim (BinOp LOr) [Pos e1, Pos e2] = whnfLogical not e1 e2
whnfPrim (BinOp op) [Pos e1, Pos e2] =
  liftA2 (,) (whnfV e1) (whnfV e2) >>= uncurry (whnfBinOp op)
whnfPrim Cond [Pos c, Pos t, Pos e] = whnfCond c t e

whnfBinOp :: BinOp -> Value -> Value -> Eval Value
whnfBinOp Lookup e1 e2 = whnfLookup e1 e2
whnfBinOp Add x@(VStr _) y = inj <$> append x y
whnfBinOp Add x y@(VStr _) = inj <$> append x y
whnfBinOp Add x@(VArr _) y@(VArr _) = liftF2 ((V.++) @Value) x y
whnfBinOp Add (VObj x) (VObj y) = x `mergeWith` y
whnfBinOp Add n1 n2 = liftF2 ((+) @Double) n1 n2
whnfBinOp Sub n1 n2 = liftF2 ((-) @Double) n1 n2
whnfBinOp Mul n1 n2 = liftF2 ((*) @Double) n1 n2
whnfBinOp Div (VNum _) (VNum 0) = throwE DivByZero
whnfBinOp Div n1 n2 = liftF2 ((/) @Double) n1 n2
whnfBinOp Mod (VNum _) (VNum 0) = throwE DivByZero
whnfBinOp Mod n1 n2 = liftF2 (mod @Int64) n1 n2
whnfBinOp Eq e1 e2 = inj <$> equals e1 e2
whnfBinOp Ne e1 e2 = inj . not <$> equals e1 e2
whnfBinOp Lt e1 e2 = liftF2 ((<) @Double) e1 e2
whnfBinOp Gt e1 e2 = liftF2 ((>) @Double) e1 e2
whnfBinOp Le e1 e2 = liftF2 ((<=) @Double) e1 e2
whnfBinOp Ge e1 e2 = liftF2 ((>=) @Double) e1 e2
whnfBinOp And e1 e2 = liftF2 ((.&.) @Int64) e1 e2
whnfBinOp Or e1 e2 = liftF2 ((.|.) @Int64) e1 e2
whnfBinOp Xor e1 e2 = liftF2 (Bits.xor @Int64) e1 e2
whnfBinOp ShiftL e1 e2 = liftF2 (Bits.shiftL @Int64) e1 e2
whnfBinOp ShiftR e1 e2 = liftF2 (Bits.shiftR @Int64) e1 e2
whnfBinOp In s o = liftF2 (\o s -> objectHasEx o s True) o s

whnfLogical :: HasValue a => (a -> Bool) -> Value -> Value -> Eval Value
whnfLogical f e1 e2 = do
  x <- whnfV e1 >>= proj'
  if f x
    then inj <$> (whnfV e2 >>= proj' @Bool)
    else pure (inj x)

append :: Value -> Value -> Eval Text
append v1 v2 = T.append <$> toString v1 <*> toString v2

whnfUnyOp :: UnyOp -> Value -> Eval Value
whnfUnyOp Compl x = inj <$> fmap (Bits.complement @Int64) (proj' x)
whnfUnyOp LNot x = inj <$> fmap not (proj' x)
whnfUnyOp Minus x = inj <$> fmap (negate @Double) (proj' x)
whnfUnyOp Plus x = inj <$> fmap (id @Double) (proj' x)
whnfUnyOp Err x = (toString >=> throwE . RuntimeError) x

toString :: Value -> Eval Text
toString (VStr s) = pure s
toString v = toStrict . encodeToLazyText <$> manifest v

whnfCond :: Value -> Value -> Value -> Eval Value
whnfCond c e1 e2 = do
  c' <- proj' c
  if c'
    then whnfV e1
    else whnfV e2

whnfLookup :: Value -> Value -> Eval Value
whnfLookup (VObj o) (VStr s) =
  whnfV . fieldValWHNF =<< liftMaybe (NoSuchKey s) (H.lookup s o)
whnfLookup (VArr a) (VNum i)
  | isInteger i =
    whnfV =<< liftMaybe (IndexOutOfBounds i) ((a !?) =<< toBoundedInteger i)
whnfLookup (VArr _) _ =
  throwE (InvalidIndex "array index was not integer")
whnfLookup (VStr s) (VNum i)
  | isInteger i =
    liftMaybe (IndexOutOfBounds i) (f =<< bounded)
  where
    f = pure . VStr . T.singleton . T.index s
    bounded =
      toBoundedInteger i >>= \i' ->
        if T.length s - 1 < i' && i' < 0
          then Nothing
          else Just i'
whnfLookup (VStr _) _ =
  throwE (InvalidIndex "string index was not integer")
whnfLookup v _ = throwTypeMismatch "array/object/string" v

whnfIndir :: Ref -> Eval Value
whnfIndir ref = do
  c <- liftIO $ readIORef ref
  case c of
    Cell v True ->
      return v -- Already evaluated, just return it
    Cell v False -> do
      v' <- whnfV v -- Needs to be reduced
      liftIO $ writeIORef ref (Cell v' True)
      return v'

whnfLetrec :: Let -> Eval Value
whnfLetrec bnd = mdo
  (r, e1) <- unbind bnd
  bnds <-
    M.fromList
      <$> mapM
        ( \(n, Embed e) -> do
            v <- extendEnv bnds (mkValue e)
            pure (n, v)
        )
        (unrec r)
  extendEnv bnds (mkValue e1)

whnfObj :: [CField] -> Eval Value
whnfObj xs = mdo
  obj <-
    mkIndirV . VObj . H.fromList . catMaybes
      =<< mapM
        ( \field ->
            let self = M.singleton (s2n "self") obj
             in whnfField self field
        )
        xs
  pure obj

whnfField ::
  -- | self object
  Env ->
  -- |
  CField ->
  -- |
  Eval (Maybe (Text, VField))
whnfField self (CField k v h) = do
  let fieldVis = h
  fieldKey <- whnf k -- keys are strictly evaluated
  fieldValWHNF <- extendEnv self (mkValue v)
  fieldVal <- extendEnv self (mkThunk v)
  fmap (,VField {..}) <$> proj' fieldKey

flattenArrays :: Vector (Vector Value) -> Vector Value
flattenArrays = join

whnfComp ::
  Comp ->
  Core ->
  Eval Value
whnfComp (ArrC bnd) cs = do
  xs <- comp
  liftF flattenArrays $ VArr $ V.mapMaybe id xs
  where
    comp =
      whnf cs >>= \case
        VArr xs -> forM xs $ \x -> do
          (n, (e, cond)) <- unbind bnd
          extendEnv (M.fromList [(n, x)]) $ do
            b <- f cond
            if b
              then Just <$> mkValue e
              else pure Nothing
        v -> throwTypeMismatch "" v
      where
        f Nothing = pure True
        f (Just c) = proj' =<< whnf c
whnfComp (ObjC bnd) cs = do
  xs <- comp
  pure $ VObj $ H.fromList $ catMaybes $ V.toList xs
  where
    comp =
      whnf cs >>= \case
        VArr xs -> forM xs $ \x -> do
          (n, (CField k v h, cond)) <- unbind bnd
          extendEnv (M.fromList [(n, x)]) $ do
            b <- f cond
            if b
              then do
                fieldKey <- whnf k
                fieldValWHNF <- mkValue v
                fieldVal <- mkThunk v
                let fieldVis = h
                fmap (,VField {..}) <$> proj' fieldKey
              else pure Nothing
        v -> throwTypeMismatch "array" v
    f Nothing = pure True
    f (Just c) = proj' =<< whnf c

-- | Right-biased union of two objects, i.e. '{x : 1} + {x : 2} == {x : 2}'
--   with OO-like `self` and `super` support via value recursion (knot-tying)
mergeWith :: Object -> Object -> Eval Value
mergeWith xs ys = mdo
  zs' <- mkIndirV $ VObj (H.unionWith f xs' ys')
  xs' <- for xs (update self zs')
  ys' <- do
    xs'' <- mkIndirV (VObj xs')
    ys'' <- for ys (update self zs')
    for ys'' (update super xs'')
  pure zs'
  where
    self = s2n "self"
    super = s2n "super"
    f a b
      | hidden a && visible b = a
      | otherwise = b
    update name xs f@VField {..} = case fieldVal of
      VThunk c env -> do
        let env' = M.insert name xs env
        let fieldVal = VThunk c env'
        fieldValWHNF <- mkIndirV fieldVal
        pure VField {..}
      _ -> pure f

visibleKeys :: Object -> HashMap Text Value
visibleKeys = H.mapMaybe f
  where
    f v@VField {..}
      | not (hidden v) = Just fieldValWHNF
      | otherwise = Nothing

liftMaybe :: EvalError -> Maybe a -> Eval a
liftMaybe e =
  \case
    Nothing -> throwE e
    Just a -> pure a

manifest :: Value -> Eval JSON.Value
manifest = \case
  VNull -> pure JSON.Null
  VBool b -> pure (JSON.Bool b)
  VStr s -> pure (JSON.String s)
  VNum n -> pure (JSON.Number n)
  VObj vs -> JSON.Object . KeyMap.fromHashMapText <$> mapM manifest (visibleKeys vs)
  VArr vs -> JSON.Array <$> mapM manifest vs
  VClos {} -> throwE (ManifestError "function")
  VFun _ -> throwE (ManifestError "function")
  v@VThunk {} -> whnfV v >>= manifest
  v@VIndir {} -> whnfV v >>= manifest
  _ -> throwE (ManifestError "impossible")

objectFieldsEx :: Object -> Bool -> [Text]
objectFieldsEx o True = L.sort (H.keys o) -- all fields
objectFieldsEx o False = L.sort $ H.keys $ H.filter (not . hidden) o -- only visible (incl. forced)

objectHasEx :: Object -> Text -> Bool -> Bool
objectHasEx o f all = f `elem` objectFieldsEx o all

primitiveEquals :: Value -> Value -> Eval Bool
primitiveEquals VNull VNull = pure True
primitiveEquals (VBool a) (VBool b) = pure (a == b)
primitiveEquals (VStr a) (VStr b) = pure (a == b)
primitiveEquals (VNum a) (VNum b) = pure (a == b)
primitiveEquals a b =
  throwE
    ( StdError "primitiveEquals operates on primitive types "
    --  <> showTy a
    --  <> showTy b
    )

equals :: Value -> Value -> Eval Bool
equals e1 e2 = liftA2 (,) (whnfV e1) (whnfV e2) >>= uncurry go
  where
    go as@(VArr a) bs@(VArr b)
      | P.length a == P.length b = do
        as' <- proj' as
        bs' <- proj' bs
        allM (uncurry equals) (zip as' bs')
      | P.length a /= P.length b = pure False
    go (VObj a) (VObj b) = do
      let fields = objectFieldsEx a False
      if fields /= objectFieldsEx b False
        then pure False
        else allM objectFieldEquals fields
      where
        objectFieldEquals field =
          let a' = fieldValWHNF (a H.! field)
              b' = fieldValWHNF (b H.! field)
           in equals a' b'
    go a b = do
      ta <- showTy a
      tb <- showTy b
      if ta == tb
        then primitiveEquals a b
        else pure False

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = foldrM (\a b -> (&& b) <$> p a) True

-- better names?
liftF ::
  (HasValue a, HasValue b) =>
  (a -> b) ->
  (Value -> Eval Value)
liftF f v = inj . f <$> proj' v
{-# INLINE liftF #-}

liftF2 ::
  (HasValue a, HasValue b, HasValue c) =>
  (a -> b -> c) ->
  Value ->
  Value ->
  Eval Value
liftF2 f v1 v2 = inj <$> liftA2 f (proj' v1) (proj' v2)
{-# INLINE liftF2 #-}

proj' :: HasValue a => Value -> Eval a
proj' = whnfV >=> proj
{-# INLINE proj' #-}

throwTypeMismatch :: Text -> Value -> Eval a
throwTypeMismatch e = throwE . TypeMismatch e <=< showTy

showTy :: Value -> Eval Text
showTy = \case
  VNull -> pure "null"
  VNum _ -> pure "number"
  VBool _ -> pure "boolean"
  VStr _ -> pure "string"
  VObj _ -> pure "object"
  VArr _ -> pure "array"
  VClos {} -> pure "function"
  VFun _ -> pure "function"
  VPrim _ -> pure "function"
  VThunk {} -> pure "thunk"
  VIndir {} -> pure "pointer"

--v@VThunk {} -> whnfV v >>= showTy
--v@VIndir {} -> whnfV v >>= showTy

instance HasValue Bool where
  proj (VBool n) = pure n
  proj v = throwTypeMismatch "bool" v
  inj = VBool
  {-# INLINE inj #-}

instance HasValue Text where
  proj (VStr s) = pure s
  proj v = throwTypeMismatch "string" v
  inj = VStr
  {-# INLINE inj #-}

instance {-# OVERLAPPING #-} HasValue [Char] where
  proj (VStr s) = pure $ T.unpack s
  proj v = throwTypeMismatch "string" v
  inj = VStr . T.pack
  {-# INLINE inj #-}

instance HasValue ByteString where
  proj (VStr s) = pure (encodeUtf8 s)
  proj v = throwTypeMismatch "string" v
  inj = VStr . decodeUtf8
  {-# INLINE inj #-}

instance HasValue Scientific where
  proj (VNum n) = pure n
  proj v = throwTypeMismatch "number" v
  inj = VNum
  {-# INLINE inj #-}

instance HasValue Double where
  proj (VNum n) = pure (toRealFloat n)
  proj v = throwTypeMismatch "number" v
  inj = VNum . fromFloatDigits
  {-# INLINE inj #-}

instance {-# OVERLAPS #-} Integral a => HasValue a where
  proj (VNum n) = pure (round n)
  proj v = throwTypeMismatch "number" v
  inj = VNum . fromIntegral
  {-# INLINE inj #-}

instance HasValue a => HasValue (Maybe a) where
  proj VNull = pure Nothing
  proj a = Just <$> proj' a
  inj Nothing = VNull
  inj (Just a) = inj a
  {-# INLINE inj #-}

instance {-# OVERLAPS #-} HasValue Object where
  proj (VObj o) = pure o
  proj v = throwTypeMismatch "object" v
  inj = VObj
  {-# INLINE inj #-}

instance HasValue a => HasValue (Vector a) where
  proj (VArr as) = mapM proj' as
  proj v = throwTypeMismatch "array" v
  inj as = VArr (inj <$> as)
  {-# INLINE inj #-}

instance {-# OVERLAPPABLE #-} HasValue a => HasValue [a] where
  proj = fmap V.toList . proj'
  inj = inj . V.fromList
  {-# INLINE inj #-}

instance {-# OVERLAPS #-} (HasValue a, HasValue b) => HasValue (a -> b) where
  inj f = VFun $ fmap (inj . f) . proj'
  {-# INLINE inj #-}
  proj = throwTypeMismatch "impossible"

instance {-# OVERLAPS #-} (HasValue a, HasValue b, HasValue c) => HasValue (a -> b -> c) where
  inj f = inj $ \x -> inj (f x)
  {-# INLINE inj #-}
  proj = throwTypeMismatch "impossible"

instance {-# OVERLAPS #-} (HasValue a, HasValue b) => HasValue (a -> Eval b) where
  inj f = VFun $ proj' >=> fmap inj . f
  {-# INLINE inj #-}
  proj (VFun f) = pure $ \x -> f (inj x) >>= proj'
  proj (VClos f e) = pure $ \x -> proj =<< whnfClos e f [Pos (inj x)]
  proj v = throwTypeMismatch "function" v

instance {-# OVERLAPS #-} (HasValue a, HasValue b, HasValue c) => HasValue (a -> b -> Eval c) where
  inj f = inj $ \x -> inj (f x)
  {-# INLINE inj #-}
  proj (VFun f) = pure $ \x y -> f (inj x) >>= \(VFun g) -> g (inj y) >>= proj'
  proj (VClos f env) = pure $ \x y -> proj' =<< whnfClos env f [Pos (inj x), Pos (inj y)]
  proj v = throwTypeMismatch "function" v
