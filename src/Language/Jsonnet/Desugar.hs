{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Jsonnet.Desugar (desugar) where

import Data.Fix as F
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Unbound.Generics.LocallyNameless

desugar :: HasSrcSpan a => Ann ExprF a -> Core
desugar =
  foldFix alg
    . zipWithOutermost
    . annMap srcSpan

-- annotate nodes with a boolean denoting outermost objects
zipWithOutermost :: Ann ExprF a -> Ann ExprF (a, Bool)
zipWithOutermost = annZip . inherit go False
  where
    go (Fix (AnnF (EObj _) _)) False = (True, True)
    go (Fix (AnnF (EObj _) _)) True = (False, True)
    go _ x = (False, x)

alg :: AnnF ExprF (SrcSpan, Bool) Core -> Core
alg (AnnF f (a, b)) = go b $ CLoc a <$> f
  where
    go outermost = \case
      ELit l -> CLit l
      EIdent i -> CVar (s2n i)
      EFun [] e -> CLam (mkVar "()" e)
      EFun ns e -> foldr (\n c -> CLam (mkVar n c)) e ns
      EApply e [] -> CApp e (CLit Null) -- here null plays the role of unit type
      EApply e es -> foldl CApp e es
      ELocal bnds e -> CLet $ mkLet bnds e
      EBinOp op e1 e2 -> CBinOp op e1 e2
      EUnyOp op e -> CUnyOp op e
      EIfElse c t e -> CIfElse c t e
      EIf c t -> CIfElse c t (CLit Null)
      EArr e -> CArr e
      EObj e ->
        if outermost
          then CLet $ mkLet (("$", self) :| []) (CVar (s2n "$"))
          else self
        where
          self = CObj (mkVar "self" e)
      ELookup e1 e2 -> CLookup e1 e2
      EErr e -> CErr e
      EAssert c m e ->
        CIfElse
          c
          e
          ( CErr $
              fromMaybe
                (CLit $ String "Assertion failed")
                m
          )

mkVar :: Alpha a => String -> a -> Bind Var a
mkVar = bind . s2n

mkLet bnds e =
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
