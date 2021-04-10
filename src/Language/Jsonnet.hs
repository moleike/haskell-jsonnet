{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Jsonnet
  ( JsonnetM,
    interpret,
    Config (..),
    Value (..),
    runJsonnetM,
    parse,
    evaluate,
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Map.Strict (singleton)
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import Debug.Trace
import Language.Jsonnet.Annotate
import qualified Language.Jsonnet.Check as Check
import Language.Jsonnet.Core
import qualified Language.Jsonnet.Desugar as Desugar
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Manifest (manifest)
import qualified Language.Jsonnet.Parser as Parser
import Language.Jsonnet.Pretty ()
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
    stdlib :: Thunk
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
desugar expr = pure (Desugar.desugar expr)

-- evaluate a Core expression with the implicit stdlib
evaluate :: Core -> JsonnetM JSON.Value
evaluate expr = do
  env <- singleton "std" <$> asks stdlib
  JsonnetM $
    lift $
      runEval env ((eval >=> manifest) expr)
