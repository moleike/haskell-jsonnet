{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.Jsonnet.Eval.Monad where

import Control.Arrow
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (listToMaybe)
import Debug.Trace
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import {-# SOURCE #-} Language.Jsonnet.Value (Thunk)
import Unbound.Generics.LocallyNameless

instance (Monoid w, Fresh m) => Fresh (RWST r w s m) where
  fresh = lift . fresh

newtype Eval a = Eval
  { unEval :: ExceptT Error (RWST Env () EvalState (FreshMT IO)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadWriter (),
      MonadReader Env,
      MonadError Error,
      MonadState EvalState,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadFail,
      Fresh
    )

type Ctx = Map (Name Core) Thunk

extendCtx :: Ctx -> Eval a -> Eval a
extendCtx ctx' =
  local
    ( \env@Env {ctx} ->
        env {ctx = M.union ctx' ctx}
    )

pushScope :: Name Core -> Eval a -> Eval a
pushScope name =
  local
    ( \env@Env {scopes} ->
        env {scopes = Just name : scopes}
    )

pushSpan :: Maybe SrcSpan -> Eval a -> Eval a
pushSpan span c = do
  local
    ( \env@Env {spans} ->
        env {spans = span : spans}
    )
    c

withCtx :: Ctx -> Eval a -> Eval a
withCtx ctx = local (\env -> env {ctx = ctx})

data Env = Env
  { ctx :: Ctx,
    spans :: [Maybe SrcSpan],
    scopes :: [Maybe (Name Core)]
  }

withEnv :: Env -> Eval a -> Eval a
withEnv rho = local (const rho)

emptyEnv :: Env
emptyEnv = Env M.empty [] [Nothing]

data EvalState = EvalState
  { currentPos :: Maybe SrcSpan
  }

emptyState :: EvalState
emptyState = EvalState Nothing

--  traceShowM $ "length of spans: "
--  traceShowM $ length sp
--  traceShowM $ "length of scopes: "
--  traceShowM $ length sc
getBacktrace :: Eval (Backtrace Core)
getBacktrace = do
  sp <- (:) <$> gets currentPos <*> asks spans
  sc <- asks scopes
  pure $
    Backtrace $
      case sequence sp of
        Just sp -> zipWith StackFrame sc sp
        Nothing -> []

throwE :: EvalError -> Eval a
throwE e = throwError . EvalError e =<< getBacktrace

runEval :: Env -> Eval a -> ExceptT Error IO a
runEval env = mapExceptT f . unEval
  where
    f comp = do
      (a, _, _) <- runFreshMT $ runRWST comp env emptyState
      pure a
