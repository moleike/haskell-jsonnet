{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
module Language.Jsonnet.Manifest where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Text (Text)
import Data.Vector (Vector)
import Language.Jsonnet.Error
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Value
import Language.Jsonnet.Common
import qualified Language.Jsonnet.Object as O
import Debug.Trace

manifest :: Value -> Eval JSON.Value
manifest =
  \case
    VNull -> pure JSON.Null
    VBool b -> pure $ JSON.Bool b
    VNum n -> pure $ JSON.Number n
    VStr s -> pure $ JSON.String s
    VArr a -> JSON.Array <$> forceArray a
    VObj o -> JSON.Object <$> forceObject o
    VClos {} -> throwError (ManifestError $ NotAJsonValue "function")
    VFun _ -> throwError (ManifestError $ NotAJsonValue "function")

forceArray :: Vector Thunk -> Eval (Vector JSON.Value)
forceArray = traverse (force >=> manifest)

forceObject :: Object -> Eval (HashMap Text JSON.Value)
forceObject = traverse (force >=> manifest) . visibleKeys

visibleKeys :: Object -> HashMap Text Thunk
visibleKeys o =
  H.fromList [(k, v) | ((O.Key k), vv@(O.Value v _)) <- xs, not (O.hidden vv)]
  where
    xs = H.toList o
