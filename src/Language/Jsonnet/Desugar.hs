{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Jsonnet.Desugar where

import Control.Applicative
import Data.Fix as F
import Data.Functor.Product
import Data.List.NonEmpty as NE
import Data.Typeable (Typeable)
import GHC.Generics
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)
import qualified Unbound.Generics.PermM as PermM

class Desugarer a where
  desugar :: a -> Core

instance Desugarer (Fix ExprF) where
  desugar = foldFix alg

instance HasSrcSpan a => Desugarer (Ann ExprF a) where
  desugar = foldFix go
    where
      go (AnnF f a) = alg $ CAnno (srcSpan a) <$> f

alg :: ExprF Core -> Core
alg = \case
  ELit l -> CLit l
  EIdent i -> CVar (s2n i)
  EFun ns e -> foldr (\n c -> CLam (bind' n c)) e ns
  EApply a bs -> foldl CApp a bs
  ELocal bnds e -> let' bnds e
  EBinOp op a b -> CBinOp op a b
  EUnyOp op a -> CUnyOp op a
  EIfElse c t e -> CIfElse c t e
  EIf c t -> CIfElse c t (CLit Null)
  EArr a -> CArr a
  EObj a -> CObj $ bind' "self" a
  ELookup a b -> CLookup a b
  EErr e -> CErr e

bind' :: Alpha a => String -> a -> Bind Var a
bind' = bind . s2n

let' :: NonEmpty (String, Core) -> Core -> Core
let' bnds =
  CLet
    . bind
      ( rec $
          NE.toList
            ( fmap
                ( \(n, a) ->
                    ((s2n n), Embed a)
                )
                bnds
            )
      )

--let' :: NonEmpty (String, Core) -> Core -> Core
--let' = flip $ foldr go
--  where
--    go (n, a@(CLam _)) b = CLet $ bind (rec ((s2n n), Embed a)) b
--    go (n, a@(CAnno _ (CLam _))) b = CLet $ bind (rec ((s2n n), Embed a)) b
--    go (n, a) b = CApp (CLam (bind' n b)) a
