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

type Param a = (Name a, Embed (Maybe a))

type Fun a = Bind (Rec [Param a]) a

type Let a = Bind (Rec [(Name a, Embed a)]) a

data Comp a
  = ArrC (Bind (Name a) (a, Maybe a))
  | ObjC (Bind (Name a) (Field a, Maybe a))
  deriving (Show, Typeable, Generic)

instance (Alpha a, Typeable a) => Alpha (Comp a)

data Core
  = CLoc SrcSpan Core
  | CLit Literal
  | CVar (Name Core)
  | CFun (Fun Core)
  | CApp Core (Args Core)
  | CLet (Let Core) -- letrec
  | CObj [Field Core]
  | CArr [Core]
  | CBinOp BinOp Core Core
  | CUnyOp UnyOp Core
  | CIfElse Core Core Core
  | CErr Core
  | CLookup Core Core
  | CComp (Comp Core) Core
  deriving (Show, Typeable, Generic)

instance Alpha Core

--data Params
--  = EmptyPs
--  | ConsPs (Rebind (Name Core, Embed (Maybe Core)) Params)
--  deriving (Show, Typeable, Generic)
--instance Alpha Params

instance IsString (Name Core) where
  fromString = string2Name
