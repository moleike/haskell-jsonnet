{- |
Module                  : Language.Jsonnet.Desugar
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Jsonnet.Desugar (desugar) where

import qualified Data.Bifunctor
import Data.Fix as F
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Debug.Trace
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty ()
import Language.Jsonnet.Syntax
import Text.PrettyPrint.ANSI.Leijen hiding (encloseSep, (<$>))
import Unbound.Generics.LocallyNameless

class Desugarer a where
  desugar :: a -> Core

instance Desugarer (Ann ExprF ()) where
  desugar = foldFix go . zipWithOutermost
    where
      go (AnnF f (_, b)) = alg b f

instance Desugarer (Ann ExprF SrcSpan) where
  desugar = foldFix go . zipWithOutermost
    where
      go (AnnF f (a, b)) = CLoc a (alg b f)

-- | annotate nodes with a boolean denoting outermost objects
zipWithOutermost :: Ann ExprF a -> Ann ExprF (a, Bool)
zipWithOutermost = annZip . inherit go False
  where
    go (Fix (AnnF EObj {} _)) False = (True, True)
    go (Fix (AnnF EObj {} _)) True = (False, True)
    go _ x = (False, x)

alg :: Bool -> ExprF Core -> Core
alg outermost = \case
  ELit l -> CLit l
  EIdent i -> CVar (s2n i)
  EFun ps e -> desugarFun ps e
  EApply e es -> CApp e es
  ELocal bnds e -> desugarLet bnds e
  EBinOp Mod e1 e2 ->
    -- operator % is overloaded for both modulo and string formatting
    stdFunc "mod" (Args [Pos e1, Pos e2] Lazy)
  EBinOp op e1 e2 -> desugarBinOp op e1 e2
  EUnyOp op e -> desugarUnyOp op e
  EIfElse c t e -> desugarIfElse c t e
  EIf c t -> desugarIfElse c t (CLit Null)
  EArr e -> CArr e
  EObj {..} -> desugarObj outermost locals fields
  ELookup e1 e2 -> desugarLookup e1 e2
  EIndex e1 e2 -> desugarLookup e1 e2
  EErr e -> desugarErr e
  EAssert e -> desugarAssert e
  ESlice {..} -> desugarSlice expr start end step
  EArrComp {expr, comp} -> desugarArrComp expr comp
  EObjComp {field, comp, locals} -> desugarObjComp field comp locals

desugarSlice expr start end step =
  stdFunc
    "slice"
    ( Args
        [ Pos expr,
          Pos $ maybeNull start,
          Pos $ maybeNull end,
          Pos $ maybeNull step
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

desugarObj outermost locals fields = obj
  where
    obj = CObj (desugarField <$> fields')

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
      (\(EField key val v o) -> EField key (f val) v o) <$> fields

desugarAssert :: Assert Core -> Core
desugarAssert (Assert c m e) =
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
    f CompSpec {..} e =
      CComp (ArrC (bind (s2n var) (e, ifspec))) forspec

desugarField :: EField Core -> CField
desugarField EField {..} = mkField key value' visibility
  where
    value' =
      if override
        then
          desugarIfElse
            (desugarBinOp In key super)
            (desugarBinOp Add (desugarLookup super key) value)
            value
        else value
    super = CVar $ s2n "super"

desugarObjComp EField {..} comp locals =
  CComp (ObjC (bind (s2n "arr") (kv', Nothing))) arrComp
  where
    kv' =
      desugarField
        ( EField
            { key = key',
              value = value',
              visibility,
              override
            }
        )
    bnds = NE.zip (fmap var comp) xs
    key' = desugarLet bnds key
    value' = case locals of
      [] -> desugarLet bnds value
      -- we need to nest the let bindings due to the impl.
      xs -> desugarLet bnds $ desugarLet (NE.fromList xs) value
    xs = desugarLookup (CVar $ s2n "arr") . CLit . Number . fromIntegral <$> [0 ..]
    arrComp = desugarArrComp arr comp
    arr = CArr $ NE.toList $ CVar . s2n . var <$> comp

stdFunc :: Text -> Args Core -> Core
stdFunc f =
  CApp
    ( desugarLookup
        (CVar "std")
        (CLit $ String f)
    )

desugarFun ps e =
  CLam $
    bind
      ( rec $
          fmap
            ( \(n, a) ->
                (s2n n, Embed (fromMaybe (errNotBound n) a))
            )
            ps
      )
      e
  where
    errNotBound n =
      desugarErr $
        CLit $
          String
            ( T.pack $
                show $
                  pretty $ ParamNotBound (pretty n)
            )

desugarLet bnds e =
  CLet $
    bind
      ( rec $
          toList
            ( fmap
                ( Data.Bifunctor.bimap s2n Embed
                )
                bnds
            )
      )
      e
