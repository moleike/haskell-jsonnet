{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Language.Jsonnet.Eval.Monad where

import Control.Arrow
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import {-# SOURCE #-} Language.Jsonnet.Value (Thunk)
import Unbound.Generics.LocallyNameless

newtype Eval a = Eval
  { unEval :: FreshMT (ExceptT EvalError (StateT EvalState IO)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadError EvalError,
      MonadState EvalState,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadFail,
      Fresh
    )

data EvalState = EvalState
  { ctx :: Map Var Thunk,
    curSpan :: Maybe SrcSpan
  }

emptyState :: EvalState
emptyState = EvalState M.empty Nothing

runEval ::
  EvalState ->
  Eval a ->
  ExceptT (EvalError, EvalState) IO a
runEval st comp = ExceptT $ (`evalStateT` st) $ do
  res <- runExceptT $ runFreshMT $ unEval comp
  st' <- get
  pure $ left (,st') res
