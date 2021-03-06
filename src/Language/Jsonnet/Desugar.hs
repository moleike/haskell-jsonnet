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

import Data.Fix as F
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Object
import Language.Jsonnet.Parser.SrcSpan
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
      go (AnnF f (a, b)) = alg b $ CLoc a <$> f

-- annotate nodes with a boolean denoting outermost objects
zipWithOutermost :: Ann ExprF a -> Ann ExprF (a, Bool)
zipWithOutermost = annZip . inherit go False
  where
    go (Fix (AnnF (EObj {}) _)) False = (True, True)
    go (Fix (AnnF (EObj {}) _)) True = (False, True)
    go _ x = (False, x)

alg outermost = \case
  ELit l -> CLit l
  EIdent i -> CVar (s2n i)
  EFun ps e -> mkFun ps e
  EApply e es -> CApp e es
  ELocal bnds e -> mkLet bnds e
  -- operator % is overloaded for both modulo and string formatting
  EBinOp (Arith Mod) e1 e2 ->
    stdFunc "mod" (Args [Pos e1, Pos e2] Lazy)
  EBinOp (Comp Eq) e1 e2 ->
    stdFunc "equals" (Args [Pos e1, Pos e2] Lazy)
  EBinOp (Comp Ne) e1 e2 ->
    CUnyOp LNot (stdFunc "equals" (Args [Pos e1, Pos e2] Lazy))
  EBinOp op e1 e2 -> CBinOp op e1 e2
  EUnyOp op e -> CUnyOp op e
  EIfElse c t e -> CIfElse c t e
  EIf c t -> CIfElse c t (CLit Null)
  EArr e -> CArr e
  EObj {..} ->
    --mkLet (("self", CObj fields) :| bnds) self
    case bnds of
      [] -> fs
      xs -> mkLet (NE.fromList xs) fs
    where
      bnds =
        if outermost
          then (("$", fs) : locals)
          else locals
      fs = CObj (mkKeyValue <$> fields)
  ELookup e1 e2 -> CLookup e1 e2
  EIndex e1 e2 -> CLookup e1 e2
  EErr e -> CErr e
  EAssert e -> mkAssert e
  ESlice {..} ->
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
  EArrComp {expr, comp} -> mkArrComp expr comp
  EObjComp {field, comp, locals} -> mkObjComp field comp locals

mkAssert :: Assert Core -> Core
mkAssert (Assert c m e) =
  CIfElse
    c
    e
    ( CErr $
        fromMaybe
          (CLit $ String "Assertion failed")
          m
    )

mkArrComp :: Core -> NonEmpty (CompSpec Core) -> Core
mkArrComp expr comp = foldr f (CArr [expr]) comp
  where
    f CompSpec {..} e =
      CComp (ArrC (bind (s2n var) (e, ifspec))) forspec

mkKeyValue :: Field Core -> KeyValue Core
mkKeyValue Field {..} = KeyValue key (Hideable value' visibility)
  where
    value' =
      if override
        then
          CIfElse
            (CBinOp In key super)
            (CBinOp (Arith Add) (CLookup super key) value)
            value
        else value
    super = CVar $ s2n "super"

mkObjComp (Field {..}) comp locals =
  CComp (ObjC (bind (s2n "arr") (kv', Nothing))) arrComp
  where
    kv' =
      mkKeyValue
        ( Field
            { key = key',
              value = value',
              visibility,
              override
            }
        )
    bnds = NE.zip (fmap var comp) xs
    key' = mkLet bnds key
    value' = case locals of
      [] -> mkLet bnds value
      -- we need to nest the let bindings due to the impl.
      xs -> mkLet bnds $ mkLet (NE.fromList xs) value
    xs = CLookup (CVar $ s2n "arr") . CLit . Number . fromIntegral <$> [0 ..]
    arrComp = mkArrComp arr comp
    arr = CArr $ NE.toList $ CVar . s2n . var <$> comp

stdFunc :: Text -> Args Core -> Core
stdFunc f =
  CApp
    ( CLookup
        (CVar "std")
        (CLit $ String f)
    )

mkFun ps e =
  CFun $
    Fun $
      bind
        ( rec $
            fmap
              ( \(n, a) ->
                  (s2n n, Embed a)
              )
              ps
        )
        e

mkLet bnds e =
  CLet $
    Let $
      bind
        ( rec $
            toList
              ( fmap
                  ( \(n, a) ->
                      (s2n n, Embed a)
                  )
                  bnds
              )
        )
        e
