-- |
module Language.Jsonnet.Syntax.Annotated where

import Data.Fix
import Data.Functor.Sum
import Data.List.NonEmpty
import Data.Semigroup.Foldable
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax

-- annotated syntax tree with resolved imports
type Expr = Ann ExprF SrcSpan

-- annotated syntax tree with unresolved imports
type Expr' = Ann ExprF' SrcSpan

mkApply :: Expr' -> [Expr'] -> Expr'
mkApply a@(Fix (AnnF _ ann)) [] =
  Fix $ AnnF (InL $ EApply a []) ann
mkApply a@(Fix (AnnF _ ann)) b =
  Fix $ AnnF (InL $ EApply a b) (fold1 (ann <| fmap attrib (fromList b)))

mkLookup :: Expr' -> Expr' -> Expr'
mkLookup a@(Fix (AnnF _ ann1)) b@(Fix (AnnF _ ann2)) =
  Fix $ AnnF (InL $ ELookup a b) (ann1 <> ann2)

mkBinOp :: BinOp -> Expr' -> Expr' -> Expr'
mkBinOp op a@(Fix (AnnF _ ann1)) b@(Fix (AnnF _ ann2)) =
  Fix $ AnnF (InL $ EBinOp op a b) (ann1 <> ann2)

-- FIXME we are manually here updating the span
mkUnyOp :: UnyOp -> Expr' -> Expr'
mkUnyOp op a@(Fix (AnnF _ ann)) =
  Fix $ AnnF (InL $ EUnyOp op a) ann
