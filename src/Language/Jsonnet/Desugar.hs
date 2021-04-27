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
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty ()
import Language.Jsonnet.Syntax
import Text.PrettyPrint.ANSI.Leijen hiding (encloseSep, (<$>))
import Unbound.Generics.LocallyNameless
import Debug.Trace

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

-- annotate nodes with a boolean denoting outermost objects
zipWithOutermost :: Ann ExprF a -> Ann ExprF (a, Bool)
zipWithOutermost = annZip . inherit go False
  where
    go (Fix (AnnF (EObj {}) _)) False = (True, True)
    go (Fix (AnnF (EObj {}) _)) True = (False, True)
    go _ x = (False, x)

alg :: Bool -> ExprF Core -> Core
alg outermost = \case
  ELit l -> CLit l
  EIdent i -> CVar (s2n i)
  EFun ps e -> mkFun ps e
  EApply e es -> CApp e es
  ELocal bnds e -> mkLet bnds e
  -- operator % is overloaded for both modulo and string formatting
  EBinOp Mod e1 e2 ->
    stdFunc "mod" (Args [Pos e1, Pos e2] Lazy)
  EBinOp Eq e1 e2 ->
    stdFunc "equals" (Args [Pos e1, Pos e2] Lazy)
  EBinOp Ne e1 e2 ->
    mkUnyOp LNot (stdFunc "equals" (Args [Pos e1, Pos e2] Lazy))
  EBinOp op e1 e2 -> mkBinOp op e1 e2
  EUnyOp op e -> mkUnyOp op e
  EIfElse c t e -> mkIfElse c t e
  EIf c t -> mkIfElse c t (CLit Null)
  EArr e -> CArr e
  EObj {..} -> mkObj outermost locals fields
  ELookup e1 e2 -> mkLookup e1 e2
  EIndex e1 e2 -> mkLookup e1 e2
  EErr e -> mkErr e
  EAssert e -> mkAssert e
  ESlice {..} -> mkSlice expr start end step
  EArrComp {expr, comp} -> mkArrComp expr comp
  EObjComp {field, comp, locals} -> mkObjComp field comp locals

mkSlice expr start end step =
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

mkIfElse :: Core -> Core -> Core -> Core
mkIfElse c t e = CApp (CPrim Cond) (Args [Pos c, Pos t, Pos e] Lazy)

mkLookup :: Core -> Core -> Core
mkLookup e1 e2 = CApp (CPrim (BinOp Lookup)) (Args [Pos e1, Pos e2] Lazy)

mkErr :: Core -> Core
mkErr e = CApp (CPrim (UnyOp Err)) (Args [Pos e] Lazy)

mkBinOp :: BinOp -> Core -> Core -> Core
mkBinOp op e1 e2 = CApp (CPrim (BinOp op)) (Args [Pos e1, Pos e2] Lazy)

mkUnyOp :: UnyOp -> Core -> Core
mkUnyOp op e = CApp (CPrim (UnyOp op)) (Args [Pos e] Lazy)

mkObj outermost locals fields =
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

mkAssert :: Assert Core -> Core
mkAssert (Assert c m e) =
  mkIfElse
    c
    e
    ( mkErr $
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
          mkIfElse
            (mkBinOp In key super)
            (mkBinOp Add (mkLookup super key) value)
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
    xs = mkLookup (CVar $ s2n "arr") . CLit . Number . fromIntegral <$> [0 ..]
    arrComp = mkArrComp arr comp
    arr = CArr $ NE.toList $ CVar . s2n . var <$> comp

stdFunc :: Text -> Args Core -> Core
stdFunc f =
  CApp
    ( mkLookup
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
                  (s2n n, Embed (fromMaybe (errNotBound n) a))
              )
              ps
        )
        e
  where
    errNotBound n =
      mkErr $
        CLit $
          String
            ( T.pack $
                show $
                  pretty $ ParamNotBound (pretty n)
            )

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
