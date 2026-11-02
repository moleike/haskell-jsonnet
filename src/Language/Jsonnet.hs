{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module                  : Language.Jsonnet
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet (
    JsonnetM,
    interpret,
    Config (..),
    defaultConfig,
    Value (..),
    runJsonnetM,
    compile,
    evaluate,
    ExtVar (..),
    ExtVarType (..),
    ExtVarContent (..),
    interpretExtVar,
    constructExtVars,
)
where

import Control.Monad (filterM, (<=<), (>=>))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM, try)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader
import Data.Aeson qualified as JSON
import Data.Binary (decode)
import Data.ByteString qualified as BS
import Data.Map.Lazy qualified as M
import Data.Map.Strict (singleton)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Language.Jsonnet.Common
import Language.Jsonnet.Compiler
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Eval.Types (ImportResult (..), Importer (..))
import Language.Jsonnet.Pretty (prettyError)
import Language.Jsonnet.Std.Lib
import Language.Jsonnet.Std.Lib qualified as Lib
import Language.Jsonnet.Std.TH (mkStdlib)
import Language.Jsonnet.Value
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))
import System.FilePath.Posix (isAbsolute)
import System.IO.Error (tryIOError)

newtype JsonnetM a = JsonnetM
    { unJsonnetM :: ReaderT Config IO a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadFix
        , MonadReader Config
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadFail
        )

data Config = Config
    { fname :: FilePath
    , extVars :: Lib.ExtVars
    , userFuncs :: Lib.UserFuncs
    , jpaths :: [FilePath]
    }

defaultConfig :: Config
defaultConfig =
    Config
        { fname = mempty
        , extVars = mempty
        , userFuncs = mempty
        , jpaths = mempty
        }

runJsonnetM :: Config -> JsonnetM a -> IO (Either Error a)
runJsonnetM conf = try . (`runReaderT` conf) . unJsonnetM

interpret :: Config -> Text -> IO (Either Error JSON.Value)
interpret conf = runJsonnetM conf . (compile >=> evaluate)

compile :: Text -> JsonnetM Core
compile inp = asks fname >>= flip compileThrow inp

-- | evaluate a Core expression with the implicit stdlib
evaluate :: Core -> JsonnetM JSON.Value
evaluate expr = do
    fp <- asks fname
    importer <- Importer . resolveImportPath <$> asks jpaths
    env <- singleton "std" <$> stdlib
    JsonnetM $ liftIO $ runEvalM fp importer env (manifest expr)

{- | the jsonnet stdlib is written in both jsonnet and Haskell, here we merge
  the native (a small subset) with the interpreted (the splice mkStdlib)
-}
stdlib :: JsonnetM Value
stdlib = do
    extVars <- asks extVars
    userFuncs <- asks userFuncs
    fp <- asks fname
    importer <- Importer . resolveImportPath <$> asks jpaths
    let lib = whnf core >>= flip mergeObjects (Lib.std extVars userFuncs)
    JsonnetM $ liftIO $ runEvalM fp importer M.empty lib
  where
    core = decode $(mkStdlib)
    mergeObjects x y = whnfPrim (BinOp Add) [Pos x, Pos y]

data ExtVar = ExtVar
    { extVarType :: !ExtVarType
    , extVarContent :: !ExtVarContent
    }

data ExtVarContent
    = Inline !Text
    | File !FilePath

data ExtVarType
    = ExtStr
    | ExtCode

interpretExtVar :: ExtVar -> IO Value
interpretExtVar = \case
    ExtVar ExtStr s -> VStr <$> readExtVarContent s
    ExtVar ExtCode s -> either dieError pure <=< interpretToValue <=< readExtVarContent $ s
  where
    readExtVarContent :: ExtVarContent -> IO Text
    readExtVarContent = \case
        Inline s -> pure s
        File p -> T.readFile p

    interpretToValue :: Text -> IO (Either Error Value)
    interpretToValue =
        runJsonnetM (defaultConfig{fname = "External variable"})
            . (compile >=> evaluateToValue)

    evaluateToValue :: Core -> JsonnetM Value
    evaluateToValue expr = do
        env <- singleton "std" <$> stdlib
        fp <- asks fname
        importer <- Importer . resolveImportPath <$> asks jpaths
        JsonnetM $ liftIO $ runEvalM fp importer env (whnf expr)

    dieError :: Error -> IO a
    dieError = die . show . prettyError

constructExtVars :: [(Text, ExtVar)] -> IO Lib.ExtVars
constructExtVars = fmap (ExtVars . M.fromList) . traverse interpretExtVarPair
  where
    interpretExtVarPair :: (Text, ExtVar) -> IO (Text, Value)
    interpretExtVarPair (s, extVar) = (s,) <$> interpretExtVar extVar

resolveImportPath :: [FilePath] -> FilePath -> String -> IO ImportResult
resolveImportPath jpaths baseFile importPath = do
    matches <- filterM doesFileExist candidates
    case matches of
        (firstMatch : _) -> do
            inp <- tryIOError $ BS.readFile firstMatch
            either (throwM . UnresolvedImport importPath) (pure . ImportResult firstMatch) inp
        [] -> throwM $ UnresolvedImport importPath (userError "Could not resolve import path")
  where
    candidates =
        if isAbsolute importPath
            then [importPath]
            else (takeDirectory baseFile </> importPath) : map (</> importPath) jpaths
