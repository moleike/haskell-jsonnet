{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module                  : Language.Jsonnet.Desugar
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Desugar (desugar) where

import Data.Bifunctor qualified
import Data.Fix as F
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty (prettyEvalError)
import Language.Jsonnet.Syntax
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
  ENull -> CLit Null
  EBool b -> CLit (Bool b)
  EStr s -> CLit (String s)
  ENum n -> CLit (Number n)
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
  EObj {..} -> desugarObj outermost locals fields asserts
  ELookup e1 i -> desugarLookup e1 (CLit (String (T.pack i)))
  EIndex e1 e2 -> desugarLookup e1 e2
  EErr e -> desugarErr e
  EAssert a e -> desugarAssert a e
  ESlice {..} -> desugarSlice expr start end step
  EArrComp {expr, comp} -> desugarArrComp expr comp
  EObjComp {field, comp, locals} -> desugarObjComp field comp locals

desugarSlice :: Core -> Maybe Core -> Maybe Core -> Maybe Core -> Core
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

desugarObjComp :: EField Core -> NonEmpty (CompSpec Core) -> [(String, Core)] -> Core
desugarObjComp EField {..} comp locals =
  CComp (ObjC (bind (s2n "arr") (kv', []))) arrComp
  where
    kv' =
      desugarField
        ( EField
            { key = key',
              value = value',
              visibility,
              override,
              computed
            }
        )
    bnds = NE.zip (fmap var comp) xs
    key' = desugarLet bnds key
    value' = case locals of
      [] -> desugarLet bnds value
      -- we need to nest the let bindings due to the impl.
      xs' -> desugarLet bnds $ desugarLet (NE.fromList xs') value
    xs = desugarLookup (CVar $ s2n "arr") . CLit . Number . fromIntegral @Integer <$> [0 ..]
    arrComp = desugarArrComp arr comp
    arr = CArr $ NE.toList $ CVar . s2n . var <$> comp

stdFunc :: Text -> Args Core -> Core
stdFunc f =
  CApp
    ( desugarLookup
        (CVar "std")
        (CLit $ String f)
    )

desugarFun :: [(String, Maybe Core)] -> Core -> Core
desugarFun ps e =
  CLam $
    bind
      ( rec $
          fmap
            ( \(n, a) ->
                (s2n n, Embed (fromMaybe (errNotBound (T.pack n)) a))
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
                  prettyEvalError $
                    ParamNotBound n
            )

desugarLet :: NonEmpty (String, Core) -> Core -> Core
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
