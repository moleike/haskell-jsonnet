{-# LANGUAGE OverloadedStrings #-}
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
    ExtVar (..),
    ExtVarType (..),
    ExtVarContent (..),
    interpretExtVar,
    constructExtVars,
  )
where

import Control.Exception (throwIO)
import Control.Monad ((>=>), (<=<))
import Control.Monad.Fix (MonadFix)
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
import qualified Data.Text.IO as T
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
import Prettyprinter (pretty)
import System.Exit (die)

newtype JsonnetM a = JsonnetM
  { unJsonnetM :: ReaderT Config (ExceptT Error IO) a
  }
  deriving newtype
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

data Config = Config
  { fname :: FilePath,
    extVars :: ExtVars
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
std = do
  extVars <- asks extVars
  let stdlib = whnf core >>= flip mergeObjects (Lib.std extVars)
  JsonnetM $ lift $ ExceptT $ runEvalM M.empty stdlib
  where
    core = decode $(mkStdlib)
    mergeObjects x y = whnfPrim (BinOp Add) [Pos x, Pos y]

data ExtVar = ExtVar
  { extVarType :: !ExtVarType,
    extVarContent :: !ExtVarContent
  }

data ExtVarContent
  = Inline !Text
  | File !FilePath

data ExtVarType
  = ExtStr
  | ExtCode

interpretExtVar :: ExtVar -> IO Value
interpretExtVar = \case
  ExtVar ExtStr s -> VStr <$> readExtVarContent s
  ExtVar ExtCode s -> either dieError pure <=< interpretToValue <=< readExtVarContent $ s
  where
    readExtVarContent :: ExtVarContent -> IO Text
    readExtVarContent = \case
      Inline s -> pure s
      File p -> T.readFile p

    interpretToValue :: Text -> IO (Either Error Value)
    interpretToValue =
      runJsonnetM (Config "External variable" mempty)
        . (parse >=> check >=> desugar >=> evaluateToValue)

    evaluateToValue :: Core -> JsonnetM Value
    evaluateToValue expr = do
      env <- singleton "std" <$> std
      JsonnetM $ lift $ ExceptT $ runEvalM env (whnf expr)

    dieError :: Error -> IO a
    dieError = die . show . pretty

constructExtVars :: [(Text, ExtVar)] -> IO ExtVars
constructExtVars = fmap (ExtVars . M.fromList) . traverse interpretExtVarPair
  where
    interpretExtVarPair :: (Text, ExtVar) -> IO (Text, Value)
    interpretExtVarPair (s, extVar) = (s,) <$> interpretExtVar extVar
