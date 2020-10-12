{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Jsonnet.Desugar where

import Data.Fix as F
import Data.List.NonEmpty as NE
import Data.Maybe
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Unbound.Generics.LocallyNameless

class Desugarer a where
  desugar :: a -> Core

instance Desugarer (Fix ExprF) where
  desugar = foldFix alg

instance HasSrcSpan a => Desugarer (Ann ExprF a) where
  desugar = foldFix go
    where
      go (AnnF f a) = alg $ CAnno (srcSpan a) <$> f

-- FIXME 'local a = b, b = c; foo' and
-- 'local a = b; local b = c; foo' should reduce to the same value,
-- but the latter will fail with unbound var b.

alg :: ExprF Core -> Core
alg = \case
  ELit l -> CLit l
  EIdent i -> CVar (s2n i)
  EFun [] e -> CLam (bind' "()" e) -- this is a workaround for no params
  EFun ns e -> foldr (\n c -> CLam (bind' n c)) e ns
  EApply a [] -> CApp a (CLit Null) -- here null plays the role of unit type
  EApply a bs -> foldl CApp a bs
  ELocal bnds e ->
    CLet $
      bind
        ( rec $
            NE.toList
              ( fmap
                  ( \(n, a) ->
                      ((s2n n), Embed a)
                  )
                  bnds
              )
        )
        e
  EBinOp op a b -> CBinOp op a b
  EUnyOp op a -> CUnyOp op a
  EIfElse c t e -> CIfElse c t e
  EIf c t -> CIfElse c t (CLit Null)
  EArr a -> CArr a
  EObj a -> CObj $ bind' "self" a
  ELookup a b -> CLookup a b
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

bind' :: Alpha a => String -> a -> Bind Var a
bind' = bind . s2n

--let' :: NonEmpty (String, Core) -> Core -> Core
--let' = flip $ foldr go
--  where
--    go (n, a@(CLam _)) b = CLet $ bind (rec ((s2n n), Embed a)) b
--    go (n, a@(CAnno _ (CLam _))) b = CLet $ bind (rec ((s2n n), Embed a)) b
--    go (n, a) b = CApp (CLam (bind' n b)) a
