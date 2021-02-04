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

withCtx :: Ctx -> Eval a -> Eval a
withCtx ctx = local (\env -> env {ctx = ctx})

data Env = Env
  { ctx :: Ctx,
    callStack :: CallStack
  }

emptyEnv :: Env
emptyEnv = Env M.empty []

data EvalState = EvalState
  { curSpan :: Maybe SrcSpan
  }

emptyState :: EvalState
emptyState = EvalState Nothing

throwE :: EvalError -> Eval a
throwE e = do
  bt <- Backtrace . Just <$> asks callStack
  sp <- gets curSpan
  throwError (EvalError e sp bt)

runEval :: Env -> Eval a -> ExceptT Error IO a
runEval env = mapExceptT f . unEval
  where
    f comp = do
      (a, _, _) <- runFreshMT $ runRWST comp env emptyState
      pure a
