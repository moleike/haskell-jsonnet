{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module                  : Language.Jsonnet.Core
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Core where

import Data.Binary (Binary)
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Jsonnet.Common (Args, Literal, Prim, Visibility)
import Language.Jsonnet.Parser.SrcSpan (SrcSpan)
import Unbound.Generics.LocallyNameless
  ( Alpha,
    Bind,
    Embed,
    Name,
    Rec,
    string2Name,
  )

type Param a = (Name a, Embed a)

data RegularField = RegularField
  { fieldKey :: Core
  , fieldVal :: Core
  , fieldVis :: Visibility
  }
  deriving stock (Show, Generic)
  deriving anyclass (Alpha, Binary)

data CField
  = CRegularField RegularField
  | CAssertField Core  -- ^ The desugared assertion expression
  deriving stock (Show, Generic)
  deriving anyclass (Alpha, Binary)

mkField :: Core -> Core -> Visibility -> CField
mkField k v h = CRegularField (RegularField k v h)

instance Binary a => Binary (Name a)

instance Binary a => Binary (Rec a)

instance Binary a => Binary (Embed a)

instance (Binary a, Binary b) => Binary (Bind a b)

data Comp
  = ArrC (Bind (Name Core) (Core, [Core]))
  | ObjC (Bind (Name Core) (CField, [Core]))
  deriving
    ( Show,
      Typeable,
      Generic,
      Alpha,
      Binary
    )

type Lam = Bind (Rec [Param Core]) Core

type Let = Bind (Rec [(Name Core, Embed Core)]) Core

data Core where
  CLoc :: SrcSpan -> Core -> Core
  CLit :: Literal -> Core
  CVar :: Name Core -> Core
  CLam :: Lam -> Core
  CPrim :: Prim -> Core
  CApp :: Core -> Args Core -> Core
  CLet :: Let -> Core
  CObj :: [CField] -> Core
  CArr :: [Core] -> Core
  CComp :: Comp -> Core -> Core
  deriving
    ( Show,
      Typeable,
      Generic,
      Alpha,
      Binary
    )

--data Params
--  = EmptyPs
--  | ConsPs (Rebind (Name Core, Embed (Maybe Core)) Params)
--  deriving (Show, Typeable, Generic)
--instance Alpha Params

instance IsString (Name Core) where
  fromString = string2Name
