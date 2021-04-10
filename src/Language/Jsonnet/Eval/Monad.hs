{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

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
import GHC.Generics
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import {-# SOURCE #-} Language.Jsonnet.Value (Thunk)
import Unbound.Generics.LocallyNameless
import Control.Lens (makeLenses, use, locally, view)

data EvalState = EvalState
  { _currentPos :: Maybe SrcSpan
  }

makeLenses ''EvalState

data CallStack = CallStack
  { _spans :: [Maybe SrcSpan],
    _scopes :: [Maybe (Name Core)]
  }

makeLenses ''CallStack

type Env = Map (Name Core) Thunk

data EvalContext = EvalContext
  { _env :: Env, -- ^ binding local variables to their values
    _callStack :: CallStack -- ^ call-stack simulation
  }

makeLenses ''EvalContext

instance (Monoid w, Fresh m) => Fresh (RWST r w s m) where
  fresh = lift . fresh

newtype Eval a = Eval
  { unEval :: ExceptT Error (RWST EvalContext () EvalState (FreshMT IO)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadWriter (),
      MonadReader EvalContext,
      MonadError Error,
      MonadState EvalState,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadFail,
      Fresh,
      Generic
    )

extendEnv :: Env -> Eval a -> Eval a
extendEnv = locally env . M.union

withEnv :: Env -> Eval a -> Eval a
withEnv = locally env . const

pushStackFrame :: (Name Core, Maybe SrcSpan) -> Eval a -> Eval a
pushStackFrame (name, span) =
  locally (callStack . scopes) (Just name :)
  . locally (callStack . spans) (span :)

emptyContext :: EvalContext
emptyContext = EvalContext M.empty emptyStack

emptyStack :: CallStack
emptyStack = CallStack [] [Nothing]

emptyState :: EvalState
emptyState = EvalState Nothing

getBacktrace :: Eval (Backtrace Core)
getBacktrace = do
  sp <- (:) <$> use currentPos <*> view (callStack . spans)
  sc <- view (callStack . scopes)
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
      (a, _, _) <- runFreshMT $ runRWST comp ctx emptyState
      pure a
    ctx = EvalContext env emptyStack
