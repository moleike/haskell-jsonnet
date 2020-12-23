{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Language.Jsonnet.Eval.Monad where

import Control.Applicative
import Control.Arrow
import Control.Monad (MonadPlus (mzero), join, msum)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import {-# SOURCE #-} Language.Jsonnet.Value (Thunk)
import Unbound.Generics.LocallyNameless

newtype Eval a = Eval
  { unEval :: LFreshMT (ExceptT EvalError (ReaderT Env (StateT EvalState IO))) a
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
      LFresh
    )

type Env = Map (Name Core) Thunk

data EvalState = EvalState
  { curSpan :: Maybe SrcSpan
  }

emptyState :: EvalState
emptyState = EvalState Nothing

runEval ::
  EvalState ->
  Env ->
  Eval a ->
  ExceptT (EvalError, EvalState) IO a
runEval st env comp = ExceptT $ (`evalStateT` st) $ do
  res <- (`runReaderT` env) $ runExceptT $ runLFreshMT $ unEval comp
  st' <- get
  pure $ left (,st') res
