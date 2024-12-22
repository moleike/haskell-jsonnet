-- |
-- Module                  : Language.Jsonnet.Check
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Check where

import Control.Monad.Except
import Data.Fix
import Data.Functor.Identity
import Data.List
import qualified Data.List.NonEmpty as NE
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common hiding (span)
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Unbound.Generics.LocallyNameless

type Check = ExceptT Error IO

check :: Ann ExprF SrcSpan -> Check ()
check = foldFixM alg
  where
    alg (AnnF f a) = withExceptT (`CheckError` (Just a)) $ case f of
      ELocal bnds _ -> checkLocal (NE.toList $ fst <$> bnds)
      EFun ps _ -> checkFun (fst <$> ps)
      EApply _ (Args as _) -> checkApply as
      _ -> pure ()
    checkLocal names = case dups names of
      [] -> pure ()
      ((x:xs) : _) -> throwError $ DuplicateBinding x
    checkFun names = case dups names of
      [] -> pure ()
      ((x:xs) : _) -> throwError $ DuplicateParam x
    checkApply args = case f args of
      [] -> pure ()
      (x : _) -> throwError $ PosAfterNamedParam
      where
        f args = filter isPos ns
        isPos = \case
          Pos _ -> True
          _ -> False
        (ps, ns) = span isPos args
    dups = filter ((> 1) . length) . group . sort
