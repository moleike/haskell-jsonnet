{-# LANGUAGE TemplateHaskell #-}

{- |
Module                  : Language.Jsonnet.Eval.Monad
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Eval.Monad where

import Control.Lens (locally, makeLenses, view)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.IORef
import Data.Map
import Data.Map.Lazy qualified as M (union)
import Language.Jsonnet.Common (Backtrace (..), Import, StackFrame (..))
import Language.Jsonnet.Core (Core)
import Language.Jsonnet.Error (Error (EvalError), EvalError)
import Language.Jsonnet.Parser.SrcSpan (SrcSpan, spanBegin)
import Text.Megaparsec.Pos
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name

type Ctx a = Map (Name Core) a

-- | Simulate a call-stack to report stack traces
data CallStack = CallStack
    { _spans :: [Maybe SrcSpan]
    -- ^ source location of call-sites
    , _scopes :: [Name Core]
    -- ^ names of called functions
    }

makeLenses ''CallStack

emptyStack :: CallStack
emptyStack = CallStack [] [s2n "top-level"]

data ImpKey = ImpKey !Import !FilePath deriving (Eq, Ord)

data EvalState a = EvalState
    { _ctx :: Ctx a
    -- ^ binding local variables to their values
    , _callStack :: CallStack
    -- ^ call-stack simulation
    , _currentPos :: Maybe SrcSpan
    -- ^ source span of expression being evaluated
    , _gen :: IORef Integer
    -- ^ fresh names
    , _currentFile :: FilePath
    -- ^ current file being evaluated
    , _loadedModules :: IORef (Map ImpKey a)
    -- ^ memoization of imports
    }

makeLenses ''EvalState

newtype EvalM a b = EvalM
    { unEval :: ExceptT Error (ReaderT (EvalState a) IO) b
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (EvalState a)
        , MonadError Error
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadFail
        , MonadFix
        )

instance Fresh (EvalM a) where
    fresh (Fn s _) = EvalM $ do
        ref <- view gen
        n <- liftIO $ readIORef ref
        liftIO $ modifyIORef' ref (+ 1)
        return $ (Fn s n)
    fresh nm@(Bn{}) = return nm

runEvalM :: FilePath -> Ctx a -> EvalM a b -> IO (Either Error b)
runEvalM fp ctx' e = do
    gen' <- newIORef 0
    mods <- newIORef empty
    let st = EvalState ctx' emptyStack Nothing gen' fp mods
    (`runReaderT` st) (runExceptT (unEval e))

throwE :: EvalError -> EvalM a b
throwE e = throwError . EvalError e =<< getBacktrace

extendEnv :: Ctx a -> EvalM a b -> EvalM a b
extendEnv = locally ctx . M.union

withEnv :: Ctx a -> EvalM a b -> EvalM a b
withEnv = locally ctx . const

withCurrentFile :: FilePath -> EvalM a b -> EvalM a b
withCurrentFile = locally currentFile . const

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

getFileName :: EvalM a FilePath
getFileName = view currentFile

getLineNum :: EvalM a (Maybe Int)
getLineNum = (unPos . sourceLine . spanBegin <$>) <$> view currentPos
