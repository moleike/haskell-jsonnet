{- |
Module                  : Language.Jsonnet.Eval.Monad
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Eval.Monad where

import Control.Lens (locally, makeLenses, view)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
  ( ExceptT (..),
    MonadError (throwError),
    MonadFix,
    MonadIO,
    runExceptT,
  )
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M (union)
import Language.Jsonnet.Common (Backtrace (..), StackFrame (..))
import Language.Jsonnet.Core (Core)
import Language.Jsonnet.Error (Error (EvalError), EvalError)
import Language.Jsonnet.Parser.SrcSpan (SrcSpan)
import Unbound.Generics.LocallyNameless
  ( Fresh,
    FreshMT,
    Name,
    runFreshMT,
    s2n,
  )

type Ctx a = Map (Name Core) a

-- | Simulate a call-stack to report stack traces
data CallStack = CallStack
  { -- | source location of call-sites
    _spans :: [Maybe SrcSpan],
    -- | names of called functions
    _scopes :: [Name Core]
  }

makeLenses ''CallStack

emptyStack :: CallStack
emptyStack = CallStack [] [s2n "top-level"]

data EvalState a = EvalState
  { -- | binding local variables to their values
    _ctx :: Ctx a,
    -- | call-stack simulation
    _callStack :: CallStack,
    -- | source span of expression being evaluated
    _currentPos :: Maybe SrcSpan
  }

makeLenses ''EvalState

newtype EvalM a b = EvalM
  { unEval :: ExceptT Error (ReaderT (EvalState a) (FreshMT IO)) b
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (EvalState a),
      MonadError Error,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadFail,
      MonadFix,
      Fresh
    )

runEvalM :: Ctx a -> EvalM a b -> IO (Either Error b)
runEvalM ctx e = runFreshMT (runReaderT (runExceptT (unEval e)) st)
  where
    st = EvalState ctx emptyStack Nothing

throwE :: EvalError -> EvalM a b
throwE e = throwError . EvalError e =<< getBacktrace

extendEnv :: Ctx a -> EvalM a b -> EvalM a b
extendEnv = locally ctx . M.union

withEnv :: Ctx a -> EvalM a b -> EvalM a b
withEnv = locally ctx . const

pushStackFrame :: (Name Core, Maybe SrcSpan) -> EvalM a b -> EvalM a b
pushStackFrame (name, span) =
  locally (callStack . scopes) (name :)
    . locally (callStack . spans) (span :)

getBacktrace :: EvalM a (Backtrace Core)
getBacktrace = do
  sp <- (:) <$> view currentPos <*> view (callStack . spans)
  sc <- view (callStack . scopes)
  pure $
    Backtrace $
      case sequence sp of
        Just sp -> zipWith StackFrame sc sp
        Nothing -> []
