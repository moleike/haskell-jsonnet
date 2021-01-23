{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
module Language.Jsonnet.Check where

import Control.Monad.Except
import Data.Fix
import Data.Functor.Identity
import Data.List
import qualified Data.List.NonEmpty as NE
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Unbound.Generics.LocallyNameless

type Check = ExceptT CheckError IO

check :: Ann ExprF SrcSpan -> Check ()
check = foldFixM alg
  where
    alg (AnnF f a) = case f of
      ELocal bnds _ -> checkLocal (NE.toList $ fst <$> bnds) a
      EFun ps _ -> checkFun (fst <$> ps) a
      EApply _ (Args as _) -> checkApply as a
      _ -> pure ()
    checkLocal names a = case dups names of
      [] -> pure ()
      (xs : _) -> throwError $ DuplicateBinding (Just a) (head xs)
    checkFun names a = case dups names of
      [] -> pure ()
      (xs : _) -> throwError $ DuplicateParam (Just a) (head xs)
    checkApply args a = case f args of
      [] -> pure ()
      (x : _) -> throwError $ PosAfterNamedParam (Just a)
      where
        f args = filter isPos ns
        isPos = \case
          Pos _ -> True
          _ -> False
        (ps, ns) = span isPos args
    dups = filter ((> 1) . length) . group . sort
