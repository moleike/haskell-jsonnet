{- |
Module                  : Language.Jsonnet.Core
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Jsonnet.Core where

import Data.Data (Data)
import Data.String
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Jsonnet.Common
import Language.Jsonnet.Parser.SrcSpan
import Unbound.Generics.LocallyNameless

type Param a = (Name a, Embed a)

data CField = CField
  { -- |
    fieldKey :: Core,
    -- |
    fieldVal :: Core,
    -- |
    fieldVis :: Visibility
  }
  deriving (Show, Generic)

mkField :: Core -> Core -> Visibility -> CField
mkField = CField

--pattern CField k v h <- CField_ k _ v h

instance Alpha CField

data Comp
  = ArrC (Bind (Name Core) (Core, Maybe Core))
  | ObjC (Bind (Name Core) (CField, Maybe Core))
  deriving (Show, Typeable, Generic)

instance Alpha Comp

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
  deriving (Show, Typeable, Generic)

instance Alpha Core

--data Params
--  = EmptyPs
--  | ConsPs (Rebind (Name Core, Embed (Maybe Core)) Params)
--  deriving (Show, Typeable, Generic)
--instance Alpha Params

instance IsString (Name Core) where
  fromString = string2Name
