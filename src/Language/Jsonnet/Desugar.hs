{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module    : Language.Jsonnet.Desugar
Copyright : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer: Alexandre Moreno <alexmorenocano@gmail.com>
Stability : experimental
Portability: non-portable
-}
module Language.Jsonnet.Desugar (desugar) where

import Control.Monad.State
import Data.Bifunctor qualified
import Data.Fix as F
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty (prettyEvalError)
import Language.Jsonnet.Syntax

type DesugarM = State Int

freshName :: DesugarM Text
freshName = do
    n <- get
    put (n + 1)
    pure $ "$" <> T.pack (show n)

class Desugarer a where
    desugarM :: a -> DesugarM Core

desugar :: (Desugarer a) => a -> Core
desugar a = evalState (desugarM a) 0

instance Desugarer (Ann ExprF ()) where
    desugarM = foldFixM go . zipWithOutermost
      where
        go (AnnF f (_, b)) = alg b f

instance Desugarer (Ann ExprF SrcSpan) where
    desugarM = foldFixM go . zipWithOutermost
      where
        go (AnnF f (a, b)) = CLoc a <$> alg b f

-- | annotate nodes with a boolean denoting outermost objects
zipWithOutermost :: Ann ExprF a -> Ann ExprF (a, Bool)
zipWithOutermost = annZip . inherit go False
  where
    go (Fix (AnnF EObj{} _)) False = (True, True)
    go (Fix (AnnF EObj{} _)) True = (False, True)
    go (Fix (AnnF EObjComp{} _)) False = (True, True)
    go (Fix (AnnF EObjComp{} _)) True = (False, True)
    go _ x = (False, x)

alg :: Bool -> ExprF Core -> DesugarM Core
alg outermost = \case
    ENull -> pure $ CLit Null
    EBool b -> pure $ CLit (Bool b)
    EStr s -> pure $ CLit (String s)
    ENum n -> pure $ CLit (Number n)
    EIdent i -> pure $ CVar (T.pack i)
    EFun ps e -> pure $ desugarFun ps e
    EApply e es -> pure $ CApp e es
    ELocal bnds e -> pure $ desugarLet bnds e
    EBinOp Mod e1 e2 ->
        -- operator % is overloaded for both modulo and string formatting
        pure $ stdFunc "mod" (Args [Pos e1, Pos e2] Lazy)
    EBinOp op e1 e2 -> pure $ desugarBinOp op e1 e2
    EUnyOp op e -> pure $ desugarUnyOp op e
    EIfElse c t e -> pure $ desugarIfElse c t e
    EIf c t -> pure $ desugarIfElse c t (CLit Null)
    EArr e -> pure $ CArr e
    EObj{..} -> pure $ desugarObj outermost locals fields asserts
    ELookup e1 i -> pure $ desugarLookup e1 (CLit (String (T.pack i)))
    EIndex e1 e2 -> pure $ desugarLookup e1 e2
    EErr e -> pure $ desugarErr e
    EAssert a e -> pure $ desugarAssert a e
    ESlice{..} -> pure $ desugarSlice expr start end step
    EArrComp{expr, comp} -> pure $ desugarArrComp expr comp
    EObjComp{field, comp, locals} -> desugarObjComp outermost field comp locals
    EImport it fp -> pure $ CImp it fp

desugarSlice :: Core -> Maybe Core -> Maybe Core -> Maybe Core -> Core
desugarSlice expr start end step =
    stdFunc
        "slice"
        ( Args
            [ Pos expr
            , Pos $ maybeNull start
            , Pos $ maybeNull end
            , Pos $ maybeNull step
            ]
            Lazy
        )
  where
    maybeNull = fromMaybe (CLit Null)

desugarIfElse :: Core -> Core -> Core -> Core
desugarIfElse c t e = CApp (CPrim Cond) (Args [Pos c, Pos t, Pos e] Lazy)

desugarLookup :: Core -> Core -> Core
desugarLookup e1 e2 = CApp (CPrim (BinOp Lookup)) (Args [Pos e1, Pos e2] Lazy)

desugarErr :: Core -> Core
desugarErr e = CApp (CPrim (UnyOp Err)) (Args [Pos e] Lazy)

desugarBinOp :: BinOp -> Core -> Core -> Core
desugarBinOp op e1 e2 = CApp (CPrim (BinOp op)) (Args [Pos e1, Pos e2] Lazy)

desugarUnyOp :: UnyOp -> Core -> Core
desugarUnyOp op e = CApp (CPrim (UnyOp op)) (Args [Pos e] Lazy)

desugarObj :: Bool -> [(String, Core)] -> [EField Core] -> [Assert Core] -> Core
desugarObj outermost locals fields asserts = obj
  where
    obj = CObj (map desugarField fields' <> map (CAssertField . flip desugarAssert (CLit (Bool True))) asserts)

    bnds =
        if outermost
            then ("$", CVar "self") : locals
            else locals

    f v@(CLit _) = v
    f v@(CLoc _ (CLit _)) = v
    f v = case bnds of
        [] -> v
        xs -> desugarLet (NE.fromList xs) v

    fields' =
        (\(EField key val comp v o) -> EField key (f val) comp v o) <$> fields

desugarAssert :: Assert Core -> Core -> Core
desugarAssert (Assert c m) e =
    desugarIfElse
        c
        e
        ( desugarErr $
            fromMaybe
                (CLit $ String "Assertion failed")
                m
        )

desugarArrComp :: Core -> NonEmpty (CompSpec Core) -> Core
desugarArrComp expr = foldr f (CArr [expr])
  where
    f CompSpec{..} e =
        CComp (ArrC (T.pack var) (e, ifspec)) forspec

desugarField :: EField Core -> CField
desugarField EField{..} = mkField key value' visibility
  where
    -- If the key is computed, use $$key inside value' so it isn't re-evaluated in valueEnv
    k = if computed then CVar "$$key" else key

    value' =
        if override
            then
                desugarIfElse
                    (desugarBinOp In k super)
                    (desugarBinOp Add (desugarLookup super k) value)
                    value
            else value
    super = CVar "super"

-- | ONLY ObjComp requires state to generate a safe `$arr_N` name
desugarObjComp :: Bool -> EField Core -> NonEmpty (CompSpec Core) -> [(String, Core)] -> DesugarM Core
desugarObjComp outermost EField{..} comp locals = do
    arrVar <- freshName
    let locals' = if outermost then ("$", CVar "self") : locals else locals

        kv' =
            desugarField
                ( EField
                    { key = key'
                    , value = value'
                    , visibility
                    , override
                    , computed
                    }
                )
        bnds = NE.zip (fmap var comp) xs
        key' = desugarLet bnds key

        value' = case locals' of
            [] -> desugarLet bnds value
            xs' -> desugarLet bnds $ desugarLet (NE.fromList xs') value

        xs = desugarLookup (CVar arrVar) . CLit . Number . fromIntegral @Integer <$> [0 ..]
        arrComp = desugarArrComp arr comp
        arr = CArr $ NE.toList $ CVar . T.pack . var <$> comp

    pure $ CComp (ObjC arrVar (kv', [])) arrComp

stdFunc :: Text -> Args Core -> Core
stdFunc f =
    CApp
        ( desugarLookup
            (CVar "std")
            (CLit $ String f)
        )

desugarFun :: [(String, Maybe Core)] -> Core -> Core
desugarFun ps e =
    CLam
        ( fmap
            ( \(n, a) ->
                (T.pack n, fromMaybe (errNotBound n) a)
            )
            ps
        )
        e
  where
    errNotBound n =
        desugarErr $
            CLit $
                String
                    ( T.show $
                        prettyEvalError $
                            ParamNotBound (T.pack n)
                    )

desugarLet :: NonEmpty (String, Core) -> Core -> Core
desugarLet bnds e =
    CLet
        ( toList
            ( fmap
                (Data.Bifunctor.first T.pack)
                bnds
            )
        )
        e
