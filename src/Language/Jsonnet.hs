{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Jsonnet where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.Map.Strict (singleton)
import Data.Text (Text)
import Language.Jsonnet.Core
import qualified Language.Jsonnet.Desugar as Desugar
import Language.Jsonnet.Error
import Language.Jsonnet.Eval (Eval, EvalState (..), eval, runEval)
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
  { fname :: FilePath
  }
  deriving (Eq, Show)

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
    go fp =
      Parser.resolveImports fp
        =<< Parser.parse fp inp

desugar :: Expr -> JsonnetM Core
desugar = pure . Desugar.desugar

runEval' :: EvalState -> Eval a -> ExceptT Error IO a
runEval' st = withExceptT mkErr . runEval st
  where
    mkErr (e, EvalState {curSpan}) = EvalError e curSpan

-- evaluate a Core expression with the implicit stdlib
evaluate :: Core -> JsonnetM JSON.Value
evaluate = JsonnetM . lift . runEval' evalSt . (eval >=> manifest)
  where
    stdlib = singleton "std" (Thunk $ pure std)
    evalSt = EvalState {ctx = stdlib, curSpan = Nothing}
