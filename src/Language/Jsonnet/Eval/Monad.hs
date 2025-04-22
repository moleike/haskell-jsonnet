{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module                  : Language.Jsonnet.Eval.Monad
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Eval.Monad where

import Control.Lens (locally, makeLenses, view)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.IORef
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as M (union)
import Language.Jsonnet.Common (Backtrace (..), StackFrame (..))
import Language.Jsonnet.Core (Core)
import Language.Jsonnet.Error (Error (EvalError), EvalError)
import Language.Jsonnet.Parser.SrcSpan (SrcSpan, spanBegin)
import Text.Megaparsec.Pos
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name

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
    _currentPos :: Maybe SrcSpan,
    -- | fresh names
    _gen :: IORef Integer
  }

makeLenses ''EvalState

newtype EvalM a b = EvalM
  { unEval :: ExceptT Error (ReaderT (EvalState a) IO) b
  }
  deriving newtype
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
      MonadFix
    )

instance Fresh (EvalM a) where
  fresh (Fn s _) = EvalM $ do
    ref <- view gen
    n <- liftIO $ readIORef ref
    liftIO $ modifyIORef' ref (+ 1)
    return $ (Fn s n)
  fresh nm@(Bn {}) = return nm

runEvalM :: Ctx a -> EvalM a b -> IO (Either Error b)
runEvalM ctx' e = do
  gen' <- newIORef 0
  let st = EvalState ctx' emptyStack Nothing gen'
  (`runReaderT` st) (runExceptT (unEval e))

throwE :: EvalError -> EvalM a b
throwE e = throwError . EvalError e =<< getBacktrace

extendEnv :: Ctx a -> EvalM a b -> EvalM a b
extendEnv = locally ctx . M.union

withEnv :: Ctx a -> EvalM a b -> EvalM a b
withEnv = locally ctx . const

pushStackFrame :: (Name Core, Maybe SrcSpan) -> EvalM a b -> EvalM a b
pushStackFrame (name, span') =
  locally (callStack . scopes) (name :)
    . locally (callStack . spans) (span' :)

getBacktrace :: EvalM a (Backtrace Core)
getBacktrace = do
  sp <- (:) <$> view currentPos <*> view (callStack . spans)
  sc <- view (callStack . scopes)
  pure $
    Backtrace $
      case sequence sp of
        Just sp' -> zipWith StackFrame sc sp'
        Nothing -> []

getFilename :: EvalM a (Maybe FilePath)
getFilename = (sourceName . spanBegin <$>) <$> view currentPos

getLineNum :: EvalM a (Maybe Int)
getLineNum = (unPos . sourceLine . spanBegin <$>) <$> view currentPos
