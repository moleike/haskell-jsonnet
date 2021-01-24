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
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
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
  { unEval :: RWST Env () EvalState (FreshMT (ExceptT EvalError IO)) a
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

throwE :: (Maybe SrcSpan -> EvalError) -> Eval a
throwE f = do
  span <- gets curSpan
  throwError (f span)

runEval :: Env -> Eval a -> ExceptT EvalError IO a
runEval env (Eval comp) = do
  (a, _, _) <- runFreshMT $ runRWST comp env emptyState
  pure a

--runEval ::
--  Env ->
--  Eval a ->
--  ExceptT Error IO a
--runEval env comp =
--  withExceptT mkErr $ ExceptT $ (`evalStateT` emptyState) $ do
--    res <- runExceptT $ (`runReaderT` env) $ runFreshMT $ unEval comp
--    st' <- get
--    pure $ left (,st') res
--  where
--    mkErr (e, EvalState {curSpan}) = EvalError e curSpan
