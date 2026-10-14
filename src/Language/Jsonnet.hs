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
    registerFun,
)
where

import Control.Monad ((<=<), (>=>))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader
import Data.Aeson qualified as JSON
import Data.Binary (decode)
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
import Language.Jsonnet.Pretty (prettyError)
import Language.Jsonnet.Std.Lib
    ( std, ExtVars(ExtVars), UserFuncs(unUserFuncs, UserFuncs) )
import Language.Jsonnet.Std.Lib qualified as Lib
import Language.Jsonnet.Std.TH (mkStdlib)
import Language.Jsonnet.Value
import System.Exit (die)

newtype JsonnetM a = JsonnetM
    { unJsonnetM :: ReaderT Config (ExceptT Error IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadFix
        , MonadReader Config
        , MonadError Error
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadFail
        )

data Config = Config
    { fname :: FilePath
    , extVars :: Lib.ExtVars
    , userFuncs :: Lib.UserFuncs
    }

defaultConfig :: Config
defaultConfig =
    Config
        { fname = ""
        , extVars = ExtVars M.empty
        , userFuncs = UserFuncs M.empty -- Empty by default
        }

runJsonnetM :: Config -> JsonnetM a -> IO (Either Error a)
runJsonnetM conf = runExceptT . (`runReaderT` conf) . unJsonnetM

interpret :: Config -> Text -> IO (Either Error JSON.Value)
interpret conf = runJsonnetM conf . (compile >=> evaluate)

compile :: Text -> JsonnetM Core
compile inp = do
    fp <- asks fname
    JsonnetM $ lift $ compileModule fp inp

-- | evaluate a Core expression with the implicit stdlib
evaluate :: Core -> JsonnetM JSON.Value
evaluate expr = do
    fp <- asks fname
    env <- singleton "std" <$> stdlib
    JsonnetM $ lift $ ExceptT $ runEvalM fp env (rnf expr)

{- | the jsonnet stdlib is written in both jsonnet and Haskell, here we merge
  the native (a small subset) with the interpreted (the splice mkStdlib)
-}
stdlib :: JsonnetM Value
stdlib = do
    extVars <- asks extVars
    userFuncs <- asks userFuncs
    fp <- asks fname
    let lib = whnf core >>= flip mergeObjects (std extVars userFuncs)
    JsonnetM $ lift $ ExceptT $ runEvalM fp M.empty lib
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
        runJsonnetM (Config "External variable" mempty mempty)
            . (compile >=> evaluateToValue)

    evaluateToValue :: Core -> JsonnetM Value
    evaluateToValue expr = do
        env <- singleton "std" <$> stdlib
        fp <- asks fname
        JsonnetM $ lift $ ExceptT $ runEvalM fp env (whnf expr)

    dieError :: Error -> IO a
    dieError = die . show . prettyError

constructExtVars :: [(Text, ExtVar)] -> IO Lib.ExtVars
constructExtVars = fmap (ExtVars . M.fromList) . traverse interpretExtVarPair
  where
    interpretExtVarPair :: (Text, ExtVar) -> IO (Text, Value)
    interpretExtVarPair (s, extVar) = (s,) <$> interpretExtVar extVar

registerFun :: HasValue a => Text -> a -> (Text, Value)
registerFun name fn = (name, inj fn)
