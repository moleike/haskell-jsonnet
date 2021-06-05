{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

data CField = CField
  { -- |
    fieldKey :: Core,
    -- |
    fieldVal :: Core,
    -- |
    fieldVis :: Visibility
  }
  deriving
    ( Show,
      Generic,
      Alpha,
      Binary
    )

mkField :: Core -> Core -> Visibility -> CField
mkField = CField

instance Binary a => Binary (Name a)

instance Binary a => Binary (Rec a)

instance Binary a => Binary (Embed a)

instance (Binary a, Binary b) => Binary (Bind a b)

data Comp
  = ArrC (Bind (Name Core) (Core, Maybe Core))
  | ObjC (Bind (Name Core) (CField, Maybe Core))
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
