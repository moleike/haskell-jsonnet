{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Language.Jsonnet.Eval.Monad where

import Control.Arrow
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import {-# SOURCE #-} Language.Jsonnet.Value (Thunk)
import Unbound.Generics.LocallyNameless

newtype Eval a = Eval
  { unEval :: FreshMT (ReaderT Env (ExceptT EvalError (StateT EvalState IO))) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadReader Env,
      MonadError EvalError,
      MonadState EvalState,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadFail,
      Fresh
    )

type Env = Map (Name Core) Thunk

emptyEnv :: Env
emptyEnv = M.empty

data EvalState = EvalState
  { curSpan :: Maybe SrcSpan
  }

emptyState :: EvalState
emptyState = EvalState Nothing

runEval ::
  Env ->
  Eval a ->
  ExceptT Error IO a
runEval env comp =
  withExceptT mkErr $ ExceptT $ (`evalStateT` emptyState) $ do
    res <- runExceptT $ (`runReaderT` env) $ runFreshMT $ unEval comp
    st' <- get
    pure $ left (,st') res
  where
    mkErr (e, EvalState {curSpan}) = EvalError e curSpan
