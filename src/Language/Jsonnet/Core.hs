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
  | CLam (Bind Var Core)
  | CApp Core Core
  | CLet (Bind (Rec [(Var, Embed Core)]) Core) -- letrec
  | CObj (Bind Var (Object Core))
  | CArr [Core]
  | CBinOp BinOp Core Core
  | CUnyOp UnyOp Core
  | CIfElse Core Core Core
  | CErr Core
  | CLookup Core Core
  deriving (Show, Typeable, Generic)

instance Alpha Core

instance IsString Var where
  fromString = string2Name
