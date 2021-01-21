{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Jsonnet
  ( JsonnetM,
    jsonnet,
    Config (..),
    Value (..),
    runJsonnetM,
    evalStd,
    parse,
    desugar,
    evaluate,
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.Map.Strict (singleton)
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import Debug.Trace
import Language.Jsonnet.Core
import qualified Language.Jsonnet.Desugar as Desugar
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Manifest (manifest)
import qualified Language.Jsonnet.Parser as Parser
import Language.Jsonnet.Pretty ()
import Language.Jsonnet.Std
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

data Config = Config
  { fname :: FilePath,
    stdlib :: Value
  }

-- the jsonnet stdlib is written in both jsonnet
-- and Haskell, here we merge the native with the
-- interpreted parts
evalStd :: IO (Either Error Value)
evalStd = runJsonnetM (Config fp VNull) comp
  where
    fp = "stdlib/std.jsonnet"
    comp = do
      inp <- liftIO $ T.readFile fp
      expr <- (parse >=> desugar) inp
      stdJ <- JsonnetM $ lift $ runEval emptyEnv (eval expr)
      JsonnetM $ lift $ runEval emptyEnv $ mergeObjects std stdJ

runJsonnetM :: Config -> JsonnetM a -> IO (Either Error a)
runJsonnetM conf = runExceptT . (`runReaderT` conf) . unJsonnetM

jsonnet :: Config -> Text -> IO (Either Error JSON.Value)
jsonnet conf = runJsonnetM conf . interpret

interpret :: Text -> JsonnetM JSON.Value
interpret = parse >=> desugar >=> evaluate

parse :: Text -> JsonnetM Expr
parse inp =
  asks fname >>= JsonnetM . lift . withExceptT ParserError . go
  where
    go fp = do
      ast <- Parser.parse fp inp
      Parser.resolveImports fp ast

desugar :: Expr -> JsonnetM Core
desugar = pure . Desugar.desugar

-- evaluate a Core expression with the implicit stdlib
evaluate :: Core -> JsonnetM JSON.Value
evaluate expr = do
  env <- singleton "std" . TV . pure <$> asks stdlib
  JsonnetM $ lift $ runEval env ((eval >=> manifest) expr)
