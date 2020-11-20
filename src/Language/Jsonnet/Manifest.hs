{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

manifest :: Value -> Eval JSON.Value
manifest =
  \case
    VNull -> pure JSON.Null
    VBool b -> pure $ JSON.Bool b
    VNum n -> pure $ JSON.Number n
    VStr s -> pure $ JSON.String s
    VArr a -> JSON.Array <$> forceArray a
    VObj o -> JSON.Object <$> forceObject o
    VClos _ _ -> throwError (ManifestError $ NotAJsonValue "function")

forceArray :: Vector Thunk -> Eval (Vector JSON.Value)
forceArray = traverse (force >=> manifest)

forceObject :: HashMap Key Thunk -> Eval (HashMap Text JSON.Value)
forceObject = traverse (force >=> manifest) . visibleKeys

visibleKeys :: HashMap Key a -> HashMap Text a
visibleKeys o = H.fromList $ do
  (Visible k, v) <- H.toList o
  pure (k, v)
