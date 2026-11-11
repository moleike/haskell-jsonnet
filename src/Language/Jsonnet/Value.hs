{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{- |
Module                  : Language.Jsonnet.Value
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Value where

import Control.Lens hiding (para)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.HashMap.Lazy as H
import Data.IORef
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import Language.Jsonnet.Common
import Language.Jsonnet.Core hiding (fieldVis, mkField)
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Eval.Types
import Language.Jsonnet.Pretty ()
import Language.Jsonnet.Object

type Eval = EvalM

data Value
    = VNull
    | VBool !Bool
    | VStr !Text
    | VNum !Scientific
    | VObj !Object !(IORef (HashMap Text Value))
    | VArr !(Vector Value)
    | VThunk !(Eval Value)
    | VIndir !(IORef Value)
    | VPrim !Prim
    | VClos !Lam !Env
    | VFun !(Value -> Eval Value)
    | VBlackhole

class HasValue a where
    inj :: a -> Value
    proj :: Value -> Eval a

instance HasValue Value where
    inj = id
    proj = pure

mkIndirV :: (MonadIO m) => Value -> m Value
mkIndirV v = VIndir <$> allocate v

mkObject :: (MonadIO m, HasValue a) => HashMap Text a -> m Value
mkObject =
    flip fmap (liftIO (newIORef H.empty))
        . VObj
        . mkBaseObject
        . H.mapWithKey mkField

allocate :: (MonadIO m) => Value -> m (IORef Value)
allocate = liftIO . newIORef

mkField :: (HasValue a) => Text -> a -> Field
mkField k v =
    Field
        { fieldKey = (VStr k)
        , fieldVal = inj v
        , fieldVis = Visible
        , assertField = False
        }
