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

import Control.Lens (locally, view)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, SomeException, fromException, throwM, try)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), local)
import Data.IORef
import Data.Map
import Data.Map.Lazy qualified as M (union)
import Data.Text qualified as T
import Language.Jsonnet.Common (Backtrace (..), StackFrame (..))
import Language.Jsonnet.Core (Core)
import Language.Jsonnet.Error (Error (EvalError), EvalError (RuntimeError))
import Language.Jsonnet.Eval.Types
import Language.Jsonnet.Parser.SrcSpan (SrcSpan, spanBegin)
import Text.Megaparsec.Pos
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name

newtype EvalM a = EvalM
    { unEval :: ReaderT EvalState IO a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader EvalState
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadFail
        , MonadFix
        )

instance Fresh EvalM where
    fresh (Fn s _) = Fn s <$> genUniqueId
    fresh nm@(Bn{}) = return nm

genUniqueId :: EvalM Integer
genUniqueId = do
    ref <- view gen
    n <- liftIO $ readIORef ref
    liftIO $ modifyIORef' ref (+ 1)
    pure n

runEvalM :: FilePath -> Importer -> Env -> EvalM a -> IO a
runEvalM fp importer' env' e = do
    gen' <- newIORef 0
    mods <- newIORef empty
    let st =
            EvalState
                { _env = env'
                , _baseEnv = env'
                , _callStack = emptyStack
                , _currentPos = Nothing
                , _gen = gen'
                , _currentFile = fp
                , _loadedModules = mods
                , _importer = importer'
                }
    runReaderT (unEval e) st

throwE :: EvalError -> EvalM a
throwE e = throwM . EvalError e =<< getBacktrace

asEvalError :: SomeException -> EvalM a
asEvalError ex = case fromException ex of
    Just (e :: Error) -> throwM e
    Nothing -> throwE (RuntimeError (T.pack (show ex)))

extendEnv :: Env -> EvalM a -> EvalM a
extendEnv = locally env . M.union

withEnv :: Env -> EvalM a -> EvalM a
withEnv = locally env . const

withCurrentFile :: FilePath -> EvalM a -> EvalM a
withCurrentFile = locally currentFile . const

withBaseEnv :: EvalM a -> EvalM a
withBaseEnv = local (\st -> st{_env = _baseEnv st})

withCurrentPos :: SrcSpan -> EvalM a -> EvalM a
withCurrentPos = locally currentPos . const . Just

emptyStack :: CallStack
emptyStack = CallStack [] [s2n "top-level"]

pushStackFrame :: (Name Core, Maybe SrcSpan) -> EvalM a -> EvalM a
pushStackFrame (name, span') =
    locally (callStack . scopes) (name :)
        . locally (callStack . spans) (span' :)

-- getBacktrace :: EvalM (Backtrace Core)
-- getBacktrace = do
--    sp <- (:) <$> view currentPos <*> view (callStack . spans)
--    sc <- view (callStack . scopes)
--    pure $
--        Backtrace $
--            case sequence sp of
--                Just sp' -> zipWith StackFrame sc sp'
--                Nothing -> []

getBacktrace :: EvalM (Backtrace Core)
getBacktrace = do
    mPos <- view currentPos
    sp <- (:) <$> view currentPos <*> view (callStack . spans)
    sc <- view (callStack . scopes)
    pure $ Backtrace $ case sequence sp of
        Just sp' -> zipWith StackFrame sc sp'
        Nothing -> case mPos of
            Just pos -> [StackFrame (s2n "top-level") pos]
            Nothing -> []

getFileName :: EvalM FilePath
getFileName = view currentFile

getLineNum :: EvalM (Maybe Int)
getLineNum = (unPos . sourceLine . spanBegin <$>) <$> view currentPos

fetchImport :: String -> EvalM ImportResult
fetchImport path = do
    cf <- view currentFile
    Importer fn <- view importer
    res <- liftIO $ try (fn cf path)
    case res of
        Left err -> throwE err
        Right result -> pure result
