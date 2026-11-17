{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module                  : Language.Jsonnet.Eval.Monad
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Eval.Monad where

import Control.Lens (locally, view, (%~), (^.))
import Control.Monad.Catch
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), local)
import Data.IORef
import Data.Map
import Data.Map.Lazy qualified as M (union)
import Data.Text qualified as T
import Language.Jsonnet.Common (Backtrace (..), StackFrame (..), Name)
import Language.Jsonnet.Core (Core)
import Language.Jsonnet.Error (Error (EvalError), EvalError (..))
import Language.Jsonnet.Eval.Types
import Language.Jsonnet.Parser.SrcSpan (SrcSpan, spanBegin)
import Text.Megaparsec.Pos

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

runEvalM :: FilePath -> Importer -> Env -> EvalM a -> IO a
runEvalM fp importer' env' e = do
    mods <- newIORef empty
    cs <- newIORef emptyStack
    pos <- newIORef Nothing
    let st =
            EvalState
                { _env = env'
                , _baseEnv = env'
                , _callStack = cs
                , _currentPos = pos
                , _currentFile = fp
                , _loadedModules = mods
                , _importer = importer'
                }
    runReaderT (unEval e) st

throwE :: EvalError -> EvalM a
throwE e = throwM . EvalError e =<< getBacktrace

liftMaybe :: EvalError -> Maybe a -> EvalM a
liftMaybe e =
    \case
        Nothing -> throwE e
        Just a -> pure a

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

emptyStack :: CallStack
emptyStack = CallStack{_spans = [], _scopes = ["top-level"]}

pushStackFrame :: (Name Core, Maybe SrcSpan) -> EvalM a -> EvalM a
pushStackFrame (name, sp) action = do
    ref <- view callStack
    old@(CallStack sps scs) <- liftIO $ readIORef ref
    liftIO $ writeIORef ref (CallStack (sp : sps) (name : scs))
    res <- action
    liftIO $ writeIORef ref old
    pure res

withCurrentPos :: SrcSpan -> EvalM a -> EvalM a
withCurrentPos sp action = do
    ref <- view currentPos
    old <- liftIO $ readIORef ref
    liftIO $ writeIORef ref (Just sp)
    res <- action
    liftIO $ writeIORef ref old
    pure res

getBacktrace :: EvalM (Backtrace Core)
getBacktrace = do
    mPos <- view currentPos >>= liftIO . readIORef
    cs   <- view callStack  >>= liftIO . readIORef
    
    let sp = mPos : (cs ^. spans)
        sc = cs ^. scopes
        
    pure $ Backtrace $ case sequence sp of
        Just sp' -> zipWith StackFrame sc sp'
        Nothing -> case mPos of
            Just pos -> [StackFrame "top-level" pos]
            Nothing -> []

getLineNum :: EvalM (Maybe Int)
getLineNum = do
    posRef <- view currentPos
    mPos <- liftIO $ readIORef posRef
    pure (unPos . sourceLine . spanBegin <$> mPos)

getFileName :: EvalM FilePath
getFileName = view currentFile

fetchImport :: String -> EvalM ImportResult
fetchImport path = do
    cf <- view currentFile
    Importer fn <- view importer
    res <- liftIO $ try (fn cf path)
    case res of
        Left err -> throwE err
        Right result -> pure result
