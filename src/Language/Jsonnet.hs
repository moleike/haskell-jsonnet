{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module                  : Language.Jsonnet
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet
  ( JsonnetM,
    interpret,
    Config (..),
    Value (..),
    runJsonnetM,
    parse,
    evaluate,
    desugar,
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.Binary (decode)
import Data.Functor.Identity
import Data.Functor.Sum
import qualified Data.Map.Lazy as M
import Data.Map.Strict (singleton)
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import Debug.Trace
import Language.Jsonnet.Annotate
import qualified Language.Jsonnet.Check as Check
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import qualified Language.Jsonnet.Desugar as Desugar
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Eval.Monad
import qualified Language.Jsonnet.Parser as Parser
import Language.Jsonnet.Pretty ()
import qualified Language.Jsonnet.Std.Lib as Lib
import Language.Jsonnet.Std.TH (mkStdlib)
import Language.Jsonnet.Syntax.Annotated
import Language.Jsonnet.Value

newtype JsonnetM a = JsonnetM
  { unJsonnetM :: ReaderT Config (ExceptT Error IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadReader Config,
      MonadError Error,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadFail
    )

newtype Config = Config
  { fname :: FilePath
  }

runJsonnetM :: Config -> JsonnetM a -> IO (Either Error a)
runJsonnetM conf = runExceptT . (`runReaderT` conf) . unJsonnetM

interpret :: Config -> Text -> IO (Either Error JSON.Value)
interpret conf =
  runJsonnetM conf
    . (parse >=> check >=> desugar >=> evaluate)

parse :: Text -> JsonnetM Expr
parse inp =
  asks fname >>= JsonnetM . lift . go
  where
    go fp = do
      ast <- Parser.parse fp inp
      Parser.resolveImports fp ast

check :: Expr -> JsonnetM Expr
check expr = do
  _ <-
    JsonnetM $
      lift $
        Check.check expr
  pure expr

desugar :: Expr -> JsonnetM Core
desugar = pure . Desugar.desugar

-- | evaluate a Core expression with the implicit stdlib
evaluate :: Core -> JsonnetM JSON.Value
evaluate expr = do
  env <- singleton "std" <$> std
  JsonnetM $ lift $ ExceptT $ runEvalM env (rnf expr)

-- | the jsonnet stdlib is written in both jsonnet and Haskell, here we merge
--   the native (a small subset) with the interpreted (the splice mkStdlib)
std :: JsonnetM Value
std = JsonnetM $ lift $ ExceptT $ runEvalM M.empty stdlib
  where
    stdlib = whnf core >>= flip mergeObjects Lib.std
    core = decode $(mkStdlib)
    mergeObjects x y = whnfPrim (BinOp Add) [Pos x, Pos y]
