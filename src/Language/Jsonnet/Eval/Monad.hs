{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Language.Jsonnet.Eval.Monad where

import Control.Arrow
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import {-# SOURCE #-} Language.Jsonnet.Value (Thunk)
import Unbound.Generics.LocallyNameless

instance (Monoid w, Fresh m) => Fresh (RWST r w s m) where
  fresh = lift . fresh

newtype Eval a = Eval
  { unEval ::
      ExceptT
        EvalError
        (RWST Env () EvalState (FreshMT IO))
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadWriter (),
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

runEval :: Env -> Eval a -> ExceptT Error IO a
runEval env = mapExceptT f . unEval
  where
    f comp = do
      (a, s, _) <- runFreshMT $ runRWST comp env emptyState
      pure $ left (`EvalError` (curSpan s)) a
