-- |
-- Module                  : Language.Jsonnet.Syntax.Annotated
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Syntax.Annotated where

import Data.Fix
import Data.Functor.Sum
import Data.List.NonEmpty
import Data.Semigroup.Foldable
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax

-- | annotated syntax tree with resolved imports
type Expr = Ann ExprF SrcSpan

-- | annotated syntax tree with unresolved imports
type Expr' = Ann ExprF' SrcSpan

mkApply :: Expr' -> Args Expr' -> Expr'
mkApply a@(Fix (AnnF _ ann)) args =
  Fix $ AnnF (InL $ EApply a args) ann

--mkApply a@(Fix (AnnF _ ann)) b =
--  Fix $ AnnF (InL $ EApply a b) (fold1 (ann <| fmap attrib (fromList b)))

mkLookup :: Expr' -> Expr' -> Expr'
mkLookup a@(Fix (AnnF _ ann1)) b@(Fix (AnnF _ ann2)) =
  Fix $ AnnF (InL $ ELookup a b) (ann1 <> ann2)

mkIndex :: Expr' -> Expr' -> Expr'
mkIndex a@(Fix (AnnF _ ann1)) b@(Fix (AnnF _ ann2)) =
  Fix $ AnnF (InL $ EIndex a b) (ann1 <> ann2)

mkSlice :: Maybe Expr' -> Maybe Expr' -> Maybe Expr' -> Expr' -> Expr'
mkSlice i n s e@(Fix (AnnF _ ann1)) =
  Fix $ AnnF (InL $ ESlice e i n s) ann1

mkBinOp :: BinOp -> Expr' -> Expr' -> Expr'
mkBinOp op a@(Fix (AnnF _ ann1)) b@(Fix (AnnF _ ann2)) =
  Fix $ AnnF (InL $ EBinOp op a b) (ann1 <> ann2)

-- FIXME we are manually here updating the span
mkUnyOp :: UnyOp -> Expr' -> Expr'
mkUnyOp op a@(Fix (AnnF _ ann)) =
  Fix $ AnnF (InL $ EUnyOp op a) ann
