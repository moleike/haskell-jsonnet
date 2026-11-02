{- |
Module                  : Language.Jsonnet.Value
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Value where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.HashMap.Lazy (HashMap)
import Data.IORef
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Language.Jsonnet.Common
import Language.Jsonnet.Core hiding (mkField)
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Eval.Types
import Language.Jsonnet.Pretty ()

type Eval = EvalM

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

data VField = VField
    { fieldKey :: Value
    , fieldValWHNF :: Value
    , fieldVal :: Value
    , fieldVis :: Visibility
    , assertField :: Bool
    }
    deriving (Generic)

type Fun = Value -> Eval Value

type Object = HashMap Text VField

data Cell = Cell {cellVal :: Value, cellIsWHNF :: Bool}
    deriving (Generic)

type Ref = IORef Cell

instance HasVisibility VField where
    visible VField{..} = fieldVis == Visible
    forced VField{..} = fieldVis == Forced
    hidden VField{..} = fieldVis == Hidden

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

mkThunk :: (MonadReader EvalState m) => Core -> m Value
mkThunk c = VThunk c <$> view env

allocate :: (MonadIO m) => Value -> m (Data.IORef.IORef Cell)
allocate = liftIO . Data.IORef.newIORef . mkCell
