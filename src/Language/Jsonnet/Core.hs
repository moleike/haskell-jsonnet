{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Jsonnet.Core where

import Data.String
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Jsonnet.Common
import Language.Jsonnet.Parser.SrcSpan
import Unbound.Generics.LocallyNameless

type Var = Name Core

data Core
  = CLoc SrcSpan Core
  | CLit Literal
  | CVar Var
  | CFun (Bind (Rec [(Var, Embed (Maybe Core))]) Core)
  | CApp Core (Args Core)
  | CLet (Bind (Rec [(Var, Embed Core)]) Core) -- letrec
  | CObj [Field Core]
  | CArr [Core]
  | CBinOp BinOp Core Core
  | CUnyOp UnyOp Core
  | CIfElse Core Core Core
  | CErr Core
  | CLookup Core Core
  deriving (Show, Typeable, Generic)

instance Alpha Core

--data Params
--  = EmptyPs
--  | ConsPs (Rebind (Var, Embed (Maybe Core)) Params)
--  deriving (Show, Typeable, Generic)
--instance Alpha Params

instance IsString Var where
  fromString = string2Name
