{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

data KeyValue a = KeyValue a (Hideable a)
  deriving (Show, Typeable, Generic)

instance Alpha a => Alpha (KeyValue a)

newtype Fun = Fun (Bind (Rec [Param Core]) Core)
  deriving (Show, Typeable, Generic)

instance Alpha Fun

newtype Let
  = Let (Bind (Rec [(Name Core, Embed Core)]) Core)
  deriving (Show, Typeable, Generic)

instance Alpha Let

data Comp
  = ArrC (Bind (Name Core) (Core, Maybe Core))
  | ObjC (Bind (Name Core) (KeyValue Core, Maybe Core))
  deriving (Show, Typeable, Generic)

instance Alpha Comp

data Core
  = CLoc SrcSpan Core
  | CLit Literal
  | CVar (Name Core)
  | CFun Fun
  | CApp Core (Args Core)
  | CLet Let
  | CObj [KeyValue Core]
  | CArr [Core]
  | CBinOp BinOp Core Core
  | CUnyOp UnyOp Core
  | CIfElse Core Core Core
  | CErr Core
  | CLookup Core Core
  | CComp Comp Core
  deriving (Show, Typeable, Generic)

instance Alpha Core

--data Params
--  = EmptyPs
--  | ConsPs (Rebind (Name Core, Embed (Maybe Core)) Params)
--  deriving (Show, Typeable, Generic)
--instance Alpha Params

instance IsString (Name Core) where
  fromString = string2Name
