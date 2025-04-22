-- |
-- Module                  : Language.Jsonnet.Value
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Value where

import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as H
import Data.IORef
import Data.Map.Strict (Map)
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Language.Jsonnet.Common
import Language.Jsonnet.Core hiding (mkField)
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Pretty ()

type Eval = EvalM Value

type Env = Ctx Value

newtype ExtVars = ExtVars (Map Text Value)
  deriving newtype (Semigroup, Monoid)

data Value
  = VNull
  | VBool !Bool
  | VStr !Text
  | VNum !Scientific
  | VObj !Object -- !Object
  | VArr !(Vector Value)
  | VThunk !Core !Env
  | VIndir !Ref
  | VPrim !Prim
  | VClos !Lam !Env
  | VFun !Fun
  | VThunk' !(Eval Value)

instance FromJSON Value where
  parseJSON = \case
    JSON.Null -> pure VNull
    JSON.Bool b -> pure $ VBool b
    JSON.Number n -> pure $ VNum n
    JSON.String s -> pure $ VStr s
    JSON.Array a -> VArr <$> traverse parseJSON a
    JSON.Object o -> VObj . f <$> traverse parseJSON (KeyMap.toHashMapText o)
    where
      f :: HashMap Text Value -> Object
      f o =
        H.fromList
          [ mkField k v
            | (k, v) <- H.toList o
          ]
      mkField k v = (k, VField (VStr k) v v Visible False)

data VField = VField
  { fieldKey :: Value,
    fieldValWHNF :: Value,
    fieldVal :: Value,
    fieldVis :: Visibility,
    assertField :: Bool
  }
  deriving (Generic)

type Fun = Value -> Eval Value

type Object = HashMap Text VField

data Cell = Cell {cellVal :: Value, cellIsWHNF :: Bool}
  deriving (Generic)

type Ref = IORef Cell

instance HasVisibility VField where
  visible VField {..} = fieldVis == Visible
  forced VField {..} = fieldVis == Forced
  hidden VField {..} = fieldVis == Hidden

class HasValue a where
  inj :: a -> Value
  proj :: Value -> Eval a

instance HasValue Value where
  inj = id
  proj = pure

mkCell :: Value -> Cell
mkCell v = Cell v False

mkIndirV :: (MonadIO m) => Value -> m Value
mkIndirV v = VIndir <$> allocate v

mkThunk :: Core -> Eval Value
mkThunk c = VThunk c <$> view ctx

allocate :: (MonadIO m) => Value -> m (IORef Cell)
allocate = liftIO . newIORef . mkCell
