{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Std
  ( std,
  )
where

import Control.Monad.Except
import Language.Jsonnet.Annotate
import Language.Jsonnet.Desugar
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Eval (mergeWith)
import Language.Jsonnet.Eval.Monad
import qualified Language.Jsonnet.Std.Lib as Lib
import Language.Jsonnet.Std.TH (mkStdlib)
import Language.Jsonnet.Value
import Prelude hiding (length)

-- the jsonnet stdlib is written in both jsonnet and Haskell, here we merge
-- the native (small, Haskell) with the interpreted (the splice mkStdlib)
std :: ExceptT Error IO Value
std = do
  ast <- pure $(mkStdlib)
  core <- pure $ desugar (annMap (const ()) ast)
  runEval emptyEnv (eval core >>= flip mergeObjects Lib.std)
  where
    mergeObjects (VObj x) (VObj y) = pure $ VObj (x `mergeWith` y)
