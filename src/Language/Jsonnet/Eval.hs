{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- |
Module      : Language.Jsonnet.Eval
Copyright   : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer  : Alexandre Moreno <alexmorenocano@gmail.com>
Stability   : experimental
Portability : non-portable
-}
module Language.Jsonnet.Eval where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens (locally, view)
import Control.Monad (foldM, forM, join, (<=<), (>=>))
import Control.Monad.Catch (handle)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (second)
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as H
import Data.HashSet qualified as HS
import Data.IORef
import Data.Int (Int64)
import Data.Map.Lazy qualified as M
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector, (!?))
import Data.Vector qualified as V
import Language.Jsonnet.Common
import Language.Jsonnet.Compiler
import Language.Jsonnet.Core hiding (fieldVal, fieldVis, mkField)
import Language.Jsonnet.Error
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Eval.Types
import Language.Jsonnet.Object as Object
import Language.Jsonnet.Pretty (ppJsonCompact)
import Language.Jsonnet.Value
import Unbound.Generics.LocallyNameless
import Prelude hiding (length)

manifest :: Core -> Eval JSON.Value
manifest = rnf >=> proj

rnf :: Core -> Eval Value
rnf = whnf >=> force

whnf :: Core -> Eval Value
whnf (CVar n) = lookupVar n
whnf (CLoc sp c) = withCurrentPos sp (whnf c)
whnf (CLit l) = pure (whnfLiteral l)
whnf (CObj bnd) = whnfObj bnd
whnf (CArr cs) = VArr . V.fromList <$> mapM mkValue cs
whnf (CLet bnd) = whnfLetrec bnd
whnf (CPrim p) = pure (VPrim p)
whnf (CApp e es) = whnfApp e es
whnf (CLam f) = VClos f <$> view env
whnf (CComp comp e) = whnfComp comp e
whnf (CImp it path) = whnfImport it path

force :: (HasValue a) => Value -> Eval a
force =
    handle asEvalError . go
  where
    go = \case
        VIndir ref -> memoizeIORef ref >>= go
        VThunk v -> v >>= go
        v -> proj v

memoize :: IO Value -> (Value -> IO ()) -> Eval Value
memoize readCache writeCache = do
    val <- liftIO readCache
    case val of
        VBlackhole -> throwE InfiniteRecursion
        VThunk{} -> go val
        v -> pure v
  where
    go thunk = do
        liftIO $ writeCache VBlackhole
        result <- force thunk
        liftIO $ writeCache result
        pure result

memoizeIORef :: IORef Value -> Eval Value
memoizeIORef = uncurry memoize . (readIORef &&& writeIORef)

memoizedLookup :: IORef (HashMap Text Value) -> Text -> Value -> Eval Value
memoizedLookup cacheRef key defVal =
    memoize
        (H.findWithDefault defVal key <$> readIORef cacheRef)
        (modifyIORef' cacheRef . H.insert key)

mkValue :: Core -> Eval Value
mkValue c@(CLit _) = whnf c
mkValue c@(CLam _) = whnf c
mkValue c@(CPrim _) = whnf c
mkValue c = mkThunk c >>= mkIndirV

mkThunk :: Core -> Eval Value
mkThunk c = do
    e <- view env
    pure $ VThunk (withEnv e (whnf c))

getSelf :: Eval Value
getSelf = view env >>= liftMaybe UnboundSelf . M.lookup (s2n "self")

getSuper :: Eval Value
getSuper = do
    rho <- view env
    case M.lookup (s2n "super") rho of
        Just v -> pure v
        Nothing ->
            if M.member (s2n "self") rho
                -- Gracefully yield an empty object to satisfy the "+:" desugaring
                then VObj (mkBaseObject H.empty) <$> liftIO (newIORef H.empty)
                else throwE UnboundSuper

lookupVar :: Name Core -> Eval Value
lookupVar = \case
    (n2s -> "self") -> getSelf
    (n2s -> "super") -> getSuper
    n -> do
        rho <- view env
        v <- liftMaybe (VarNotFound (n2s n)) (M.lookup n rho)
        force v

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
whnfApp e es = withStackFrame e $ handle asEvalError $ do
    vs <- whnfArgs es
    whnf e >>= force >>= \case
        VClos f env -> whnfClos env f vs
        VPrim op -> whnfPrim op vs
        v@(VFun _) -> foldlM f v vs
          where
            f (VFun g) (Pos v') = g v'
            f v' _ = throwTypeMismatch "function" v'
        v -> throwTypeMismatch "function" v

withStackFrame :: Core -> Eval a -> Eval a
withStackFrame (CLoc sp (CVar n)) e =
    pushStackFrame (n, Just sp) e
withStackFrame (CLoc sp _) e =
    pushStackFrame (s2n "anonymous", Just sp) e
withStackFrame (CVar _) e = e
withStackFrame _ e = e

whnfClos :: Env -> Lam -> [Arg Value] -> Eval Value
whnfClos rho f args = do
    (bnds, e) <- unbind f
    (rs, ps, ns) <- splitArgs args (second unembed <$> unrec bnds)
    withEnv rho $
        extendEnv (M.fromList ps) $
            extendEnv (M.fromList ns) $
                appDefaults rs e

appDefaults :: [(Name Core, Core)] -> Core -> Eval Value
appDefaults rs e = mdo
    bnds <-
        M.fromList
            <$> mapM
                ( \(n, e') -> do
                    th <- extendEnv bnds (mkValue e')
                    pure (n, th)
                )
                rs
    extendEnv bnds (whnf e)

splitArgs :: [Arg c] -> [(Name a, b)] -> EvalM ([(Name a, b)], [(Name a, c)], [(Name a, c)])
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

    getNamed = traverse f ns
      where
        f (a, b) = case g a of
            Nothing -> throwE $ BadParam (T.pack a)
            Just n -> pure (n, b)
        g a = find ((a ==) . name2String) pNames

    getUnapp named =
        pure $ filter ((`notElem` ns') . fst) bnds2
      where
        ns' = map fst named

    split [] = ([], [])
    split (Pos p : xs) =
        let (ys, zs) = split xs in (p : ys, zs)
    split (Named n v : xs) =
        let (ys, zs) = split xs in (ys, (n, v) : zs)

whnfPrim :: Prim -> [Arg Value] -> Eval Value
whnfPrim (UnyOp op) [Pos e] = force e >>= whnfUnyOp op
whnfPrim (BinOp LAnd) [Pos e1, Pos e2] = whnfLogical id e1 e2
whnfPrim (BinOp LOr) [Pos e1, Pos e2] = whnfLogical not e1 e2
whnfPrim (BinOp op) [Pos e1, Pos e2] =
    liftA2 (,) (force e1) (force e2) >>= uncurry (whnfBinOp op)
whnfPrim Cond [Pos c, Pos t, Pos e] = whnfCond c t e

whnfBinOp :: BinOp -> Value -> Value -> Eval Value
whnfBinOp Lookup e1 e2 = whnfLookup e1 e2
whnfBinOp Add x@(VStr _) y = inj <$> append x y
whnfBinOp Add x y@(VStr _) = inj <$> append x y
whnfBinOp Add x@(VArr _) y@(VArr _) = liftF2 ((V.++) @Value) x y
whnfBinOp Add (VObj x _) (VObj y _) = pure $ inj (x <> y)
whnfBinOp Add n1 n2 = liftF2 ((+) @Double) n1 n2
whnfBinOp Sub n1 n2 = liftF2 ((-) @Double) n1 n2
whnfBinOp Mul n1 n2 = liftF2 ((*) @Double) n1 n2
whnfBinOp Div (VNum _) (VNum 0) = throwE DivByZero
whnfBinOp Div n1 n2 = liftF2 ((/) @Double) n1 n2
whnfBinOp Mod (VNum _) (VNum 0) = throwE DivByZero
whnfBinOp Mod n1 n2 = liftF2 (mod @Int64) n1 n2
whnfBinOp Eq e1 e2 = inj <$> equals e1 e2
whnfBinOp Ne e1 e2 = inj . not <$> equals e1 e2
whnfBinOp Lt e1 e2 = inj . (== LT) <$> cmpValue e1 e2
whnfBinOp Gt e1 e2 = inj . (== GT) <$> cmpValue e1 e2
whnfBinOp Le e1 e2 = inj . (/= GT) <$> cmpValue e1 e2
whnfBinOp Ge e1 e2 = inj . (/= LT) <$> cmpValue e1 e2
whnfBinOp And e1 e2 = liftF2 ((.&.) @Int64) e1 e2
whnfBinOp Or e1 e2 = liftF2 ((.|.) @Int64) e1 e2
whnfBinOp Xor e1 e2 = liftF2 (Bits.xor @Int64) e1 e2
whnfBinOp ShiftL e1 e2 = liftF2 (Bits.shiftL @Int64) e1 e2
whnfBinOp ShiftR e1 e2 = liftF2 (Bits.shiftR @Int64) e1 e2
whnfBinOp In s o = liftF2 (\o' s' -> objectHasEx o' s' True) o s

whnfLogical :: (HasValue a) => (a -> Bool) -> Value -> Value -> Eval Value
whnfLogical f e1 e2 = do
    x <- force e1
    if f x
        then inj <$> (force @Bool) e2
        else pure (inj x)

cmpValue :: Value -> Value -> Eval Ordering
cmpValue (VNum a) (VNum b) = pure (compare a b)
cmpValue (VStr a) (VStr b) = pure (compare a b)
cmpValue (VArr a) (VArr b) = do
    as <- mapM force (V.toList a)
    bs <- mapM force (V.toList b)
    go as bs
  where
    go [] [] = pure EQ
    go [] _ = pure LT
    go _ [] = pure GT
    go (x : xs) (y : ys) = do
        ord <- cmpValue x y
        case ord of
            EQ -> go xs ys
            _ -> pure ord
cmpValue v1 _ = throwTypeMismatch "number, string, or array" v1

append :: Value -> Value -> Eval Text
append v1 v2 = T.append <$> toString v1 <*> toString v2

whnfUnyOp :: UnyOp -> Value -> Eval Value
whnfUnyOp Compl x = inj <$> fmap (Bits.complement @Int64) (force x)
whnfUnyOp LNot x = inj <$> fmap not (force x)
whnfUnyOp Minus x = inj <$> fmap (negate @Double) (force x)
whnfUnyOp Plus x = inj <$> fmap (id @Double) (force x)
whnfUnyOp Err x = (toString >=> throwE . RuntimeError) x
whnfUnyOp Super x = do
    rho <- view env
    let err =
            if M.member (s2n "self") rho
                then NoSuperClass
                else UnboundSuper

    case M.lookup (s2n "super") rho of
        Just (VObj s c) -> force x >>= objectLookup s c
        _ -> throwE err

whnfImport :: Import -> FilePath -> Eval Value
whnfImport it fp = do
    ImportResult{..} <- fetchImport fp
    let key = ImpKey it foundAt
    ref <- view loadedModules
    mods <- liftIO $ readIORef ref
    case M.lookup key mods of
        Just val -> pure val
        Nothing -> do
            val <- case it of
                Code -> do
                    core <- compileThrow foundAt (TE.decodeUtf8 content)
                    pure $ VThunk (withCurrentFile foundAt $ withBaseEnv $ whnf core)
                Bin -> pure $ inj (BS.unpack content)
                Str -> pure $ inj (TE.decodeUtf8 content)
            ptr <- mkIndirV val
            liftIO $ writeIORef ref (M.insert key ptr mods)
            return ptr

toString :: Value -> Eval Text
toString (VStr s) = pure s
toString v = T.show . ppJsonCompact <$> force @JSON.Value v

whnfCond :: Value -> Value -> Value -> Eval Value
whnfCond c e1 e2 = do
    c' <- force c
    if c'
        then force e1
        else force e2

whnfLookup :: Value -> Value -> Eval Value
whnfLookup v@(VObj o c) (VStr s) = withSelf v (objectLookup o c s)
whnfLookup (VArr a) (VNum i)
    | isInteger i =
        force =<< liftMaybe (IndexOutOfBounds i) ((a !?) =<< toBoundedInteger i)
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
whnfObj xs = do
    fields <- foldM insertField H.empty xs
    let obj = mkBaseObject fields
    cache <- liftIO (newIORef H.empty)
    pure (VObj obj cache)
  where
    insertField acc f = foldr (uncurry H.insert) acc <$> whnfField f

extendWithLateBound :: Env -> Eval a -> Eval a
extendWithLateBound baseEnv m = do
    dynEnv <- view env
    let inherit key = M.alter (const $ M.lookup key dynEnv) key
    withEnv (inherit (s2n "self") . inherit (s2n "super") $ baseEnv) m

whnfField :: CField -> Eval (Maybe (Text, Field))
whnfField (CRegularField (RegularField k v vis)) = do
    fieldKey <- whnf k
    force fieldKey
        >>= traverse
            ( \key -> do
                env' <- M.insert (s2n "$$key") (VStr key) <$> view env
                let fieldVal = VThunk $ extendWithLateBound env' (whnf v)
                pure (key, Field fieldKey fieldVal vis False)
            )
whnfField (CAssertField assertion) = do
    env' <- view env
    keyName <- nameToText <$> fresh (s2n "$$assert")
    let val = VThunk $ extendWithLateBound env' (whnf assertion)
    pure $ Just (keyName, Field (VStr keyName) val Visible True)
  where
    nameToText n = T.pack (name2String n) <> "_" <> T.pack (show (name2Integer n))

flattenArrays :: Vector (Vector Value) -> Vector Value
flattenArrays = join

whnfIfs :: [Core] -> Eval Bool
whnfIfs = foldM (\b c -> (b &&) <$> (whnf c >>= force)) True

whnfComp :: Comp -> Core -> Eval Value
whnfComp (ArrC bnd) cs = do
    xs <- comp
    liftF flattenArrays $ VArr $ V.mapMaybe id xs
  where
    comp =
        whnf cs >>= \case
            VArr xs -> forM xs $ \x -> do
                (n, (e, cond)) <- unbind bnd
                extendEnv (M.singleton n x) $ do
                    b <- whnfIfs cond
                    if b
                        then Just <$> mkValue e
                        else pure Nothing
            v -> throwTypeMismatch "array" v
whnfComp (ObjC bnd) cs = do
    xs <- mkBaseObject . H.fromList . catMaybes <$> comp
    cache <- liftIO (newIORef H.empty)
    pure (VObj xs cache)
  where
    comp =
        whnf cs >>= force >>= \case
            VArr arr -> forM (V.toList arr) $ \x -> do
                (n, (cField, cond)) <- unbind bnd
                extendEnv (M.singleton n x) $ do
                    b <- whnfIfs cond
                    if b
                        then whnfField cField
                        else pure Nothing
            v -> throwTypeMismatch "array" v

withSelf :: Value -> Eval a -> Eval a
withSelf selfVal action =
    extendEnv (M.singleton (s2n "self") selfVal) action

withSuper :: Maybe Object -> Eval a -> Eval a
withSuper (Just s) action = do
    cache <- liftIO (newIORef H.empty)
    extendEnv (M.singleton (s2n "super") (VObj s cache)) action
withSuper Nothing action =
    locally env (M.delete (s2n "super")) action

evalField :: Object -> Text -> Value
evalField obj key = VThunk $ do
    (field, superTree) <- liftMaybe (NoSuchKey key) (Object.lookupField key obj)
    withSuper superTree (force $ fieldVal field)

objectLookup o c = memoizedLookup c <*> evalField o

objectFieldsEx :: Object -> Bool -> [Text]
objectFieldsEx o = \case
    True -> Object.regularKeys o
    False -> Object.visibleKeys o

objectHasEx :: Object -> Text -> Bool -> Bool
objectHasEx o f all' = f `elem` objectFieldsEx o all'

primitiveEquals :: Value -> Value -> Eval Bool
primitiveEquals VNull VNull = pure True
primitiveEquals (VBool a) (VBool b) = pure (a == b)
primitiveEquals (VStr a) (VStr b) = pure (a == b)
primitiveEquals (VNum a) (VNum b) = pure (a == b)
primitiveEquals _ _ =
    throwE
        (StdError "primitiveEquals operates on primitive types ")

equals :: Value -> Value -> Eval Bool
equals e1 e2 = do
    v1 <- force e1
    v2 <- force e2
    go v1 v2
  where
    go (VArr a) (VArr b)
        | V.length a == V.length b =
            allM (uncurry equals) (V.zip a b)
        | otherwise = pure False
    go (VObj a c1) (VObj b c2) = do
        let keysA = Object.visibleKeys a
            keysB = Object.visibleKeys b
        if HS.fromList keysA /= HS.fromList keysB
            then pure False
            else do
                aOk <- checkAssertions a c1
                bOk <- checkAssertions b c2
                if not (aOk && bOk)
                    then pure False
                    else allM compareField keysA
      where
        compareField k = do
            va <- withSelf (VObj a c1) $ objectLookup a c1 k
            vb <- withSelf (VObj b c2) $ objectLookup b c2 k
            equals va vb
    go a b = do
        ta <- showTy a
        tb <- showTy b
        if ta == tb
            then primitiveEquals a b
            else pure False

    checkAssertions :: Object -> IORef (HashMap Text Value) -> Eval Bool
    checkAssertions obj cache = do
        let selfVal = VObj obj cache
        allM
            ( \k ->
                -- Use withSelf and objectLookup directly!
                withSelf selfVal (objectLookup obj cache k) >>= force >>= \case
                    VBool b -> pure b
                    _ -> pure True
            )
            (Object.assertionKeys obj)

allM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
allM p = foldrM (\a b -> (&& b) <$> p a) True

liftF ::
    (HasValue a, HasValue b) =>
    (a -> b) ->
    (Value -> Eval Value)
liftF f v = inj . f <$> force v
{-# INLINE liftF #-}

liftF2 ::
    (HasValue a, HasValue b, HasValue c) =>
    (a -> b -> c) ->
    Value ->
    Value ->
    Eval Value
liftF2 f v1 v2 = inj <$> liftA2 f (force v1) (force v2)
{-# INLINE liftF2 #-}

throwTypeMismatch :: Text -> Value -> Eval a
throwTypeMismatch e = throwE . TypeMismatch e <=< showTy

showTy :: Value -> Eval Text
showTy = \case
    VNull -> pure "null"
    VNum _ -> pure "number"
    VBool _ -> pure "boolean"
    VStr _ -> pure "string"
    VObj _ _ -> pure "object"
    VArr _ -> pure "array"
    VClos{} -> pure "function"
    VFun _ -> pure "function"
    VPrim _ -> pure "function"
    VThunk{} -> pure "thunk"
    VIndir{} -> pure "pointer"

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

instance {-# OVERLAPS #-} (Integral a) => HasValue a where
    proj (VNum n) = pure (round n)
    proj v = throwTypeMismatch "number" v
    inj = VNum . fromIntegral
    {-# INLINE inj #-}

instance (HasValue a) => HasValue (Maybe a) where
    proj VNull = pure Nothing
    proj a = Just <$> force a
    inj Nothing = VNull
    inj (Just a) = inj a
    {-# INLINE inj #-}

instance {-# OVERLAPS #-} HasValue Object where
    proj (VObj o _) = pure o
    proj v = throwTypeMismatch "object" v
    inj o = VThunk $ liftIO $ VObj o <$> (newIORef H.empty)
    {-# INLINE inj #-}

instance (HasValue a) => HasValue (Vector a) where
    proj (VArr as) = mapM force as
    proj v = throwTypeMismatch "array" v
    inj as = VArr (inj <$> as)
    {-# INLINE inj #-}

instance {-# OVERLAPPABLE #-} (HasValue a) => HasValue [a] where
    proj = fmap V.toList . force
    inj = inj . V.fromList
    {-# INLINE inj #-}

instance (HasValue a) => HasValue (HashMap Text a) where
    inj = VThunk . mkObject
    proj selfVal@(VObj obj cache) = do
        let keys = Object.visibleKeys obj
        pairs <- forM keys $ \k -> do
            val <- force =<< withSelf selfVal (objectLookup obj cache k)
            pure (k, val)
        pure (H.fromList pairs)
    proj v = throwTypeMismatch "object" v

instance {-# OVERLAPS #-} (HasValue a, HasValue b) => HasValue (a -> b) where
    inj f = VFun $ fmap (inj . f) . force
    {-# INLINE inj #-}
    proj = throwTypeMismatch "impossible"

instance {-# OVERLAPS #-} (HasValue a, HasValue b, HasValue c) => HasValue (a -> b -> c) where
    inj f = inj $ \x -> inj (f x)
    {-# INLINE inj #-}
    proj = throwTypeMismatch "impossible"

instance {-# OVERLAPS #-} (HasValue a) => HasValue (Eval a) where
    inj a = VThunk $ inj <$> a
    proj = force

instance {-# OVERLAPS #-} (HasValue a, HasValue b) => HasValue (a -> Eval b) where
    inj f = VFun $ force >=> fmap inj . f
    {-# INLINE inj #-}
    proj (VFun f) = pure $ \x -> f (inj x) >>= force
    proj (VClos f e) = pure $ \x -> proj =<< whnfClos e f [Pos (inj x)]
    proj v = throwTypeMismatch "function" v

instance {-# OVERLAPS #-} (HasValue a, HasValue b, HasValue c) => HasValue (a -> b -> Eval c) where
    inj f = inj $ \x -> inj (f x)
    {-# INLINE inj #-}
    proj (VFun f) = pure $ \x y -> f (inj x) >>= \(VFun g) -> g (inj y) >>= force
    proj (VClos f env) = pure $ \x y -> force =<< whnfClos env f [Pos (inj x), Pos (inj y)]
    proj v = throwTypeMismatch "function" v

instance HasValue JSON.Value where
    inj = \case
        JSON.Null -> VNull
        JSON.Bool b -> inj b
        JSON.Number n -> inj n
        JSON.String s -> inj s
        JSON.Array a -> inj a
        JSON.Object o -> inj (KeyMap.toHashMapText o)

    proj = \case
        VNull -> pure JSON.Null
        VBool b -> pure (JSON.Bool b)
        VStr s -> pure (JSON.String s)
        VNum n -> pure (JSON.Number n)
        VArr vs -> JSON.Array <$> proj (VArr vs)
        VObj vs c -> JSON.Object . KeyMap.fromHashMapText <$> proj (VObj vs c)
        VClos{} -> throwE (ManifestError "function")
        VFun{} -> throwE (ManifestError "function")
        _ -> throwE (ManifestError "impossible")
