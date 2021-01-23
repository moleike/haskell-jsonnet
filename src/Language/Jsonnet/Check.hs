{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
module Language.Jsonnet.Check where

import Language.Jsonnet.Core
import Language.Jsonnet.Syntax
import Language.Jsonnet.Error
import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Data.Functor.Identity
import Language.Jsonnet.Annotate
import Language.Jsonnet.Parser.SrcSpan
import Data.Fix
import Data.List

type Check = ExceptT CheckError IO

check :: Ann ExprF SrcSpan -> Check ()
check = foldFixM alg
  where
    alg (AnnF f a)= case f of
      EFun ps _ -> checkFun (fst <$> ps) a
      EApply _ es -> pure ()
      ELocal bnds _ -> pure ()
      _ -> pure ()
    checkFun names a = case f names of
      [] -> pure ()
      (xs: _) -> throwError $ DuplicateParam (Just a) (head xs)
      where
        f = filter ((> 1) . length) . group . sort
