{- |
Module                  : Language.Jsonnet.Check
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Check where

import Control.Monad.Except (MonadError, throwError)
import Data.Fix ( foldFixM )
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common hiding (span)
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Data.Foldable (for_)

check :: (MonadError Error m) => Ann ExprF SrcSpan -> m ()
check = foldFixM alg
  where
    alg (AnnF f a) = for_ (checkNode f) $ \err ->
        throwError $ CheckError err (Just a)

    checkNode = \case
        ELocal bnds _ -> checkLocal (NE.toList $ fst <$> bnds)
        EFun ps _ -> checkFun (fst <$> ps)
        EApply _ (Args as _) -> checkApply as
        EObj{..} -> checkLocal (fst <$> locals)
        _ -> Nothing

    checkLocal :: [Ident] -> Maybe CheckError
    checkLocal names = case dups names of
        ((x : _) : _) -> Just $ DuplicateBinding x
        _ -> Nothing

    checkFun :: [Ident] -> Maybe CheckError
    checkFun names = case dups names of
        ((x : _) : _) -> Just $ DuplicateParam x
        _ -> Nothing

    checkApply :: [Arg a] -> Maybe CheckError
    checkApply args = case f args of
        [] -> Nothing
        _ -> Just PosAfterNamedParam
      where
        f _ = filter isPos ns
        isPos = \case
            Pos _ -> True
            _ -> False
        (_, ns) = span isPos args

    dups = filter ((> 1) . length) . List.group . List.sort
