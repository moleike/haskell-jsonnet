{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Common where

import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.TH (makeClosedAlpha)
import Data.Scientific (Scientific)

data Literal = Null | Bool Bool | String Text | Number Scientific
  deriving (Show, Eq, Ord, Generic)

makeClosedAlpha ''Literal

instance Subst a Literal where
  subst _ _ = id
  substs _ = id

data BinOp
  = Arith ArithOp
  | Comp CompOp
  | Bitwise BitwiseOp
  | Logical LogicalOp
  deriving (Eq, Show, Generic)

data UnyOp
  = Compl
  | LNot
  | Plus
  | Minus
  deriving (Show, Eq, Enum, Bounded, Generic)

data ArithOp = Add | Sub | Mul | Div | Mod
  deriving (Show, Eq, Enum, Bounded, Generic)

data CompOp = Lt | Le | Gt | Ge | Eq | Ne
  deriving (Show, Eq, Enum, Bounded, Generic)

data BitwiseOp = And | Or | Xor | ShiftL | ShiftR
  deriving (Show, Eq, Enum, Bounded, Generic)

data LogicalOp = LAnd | LOr
  deriving (Show, Eq, Enum, Bounded, Generic)

instance Alpha ArithOp

instance Alpha BinOp

instance Alpha CompOp

instance Alpha BitwiseOp

instance Alpha LogicalOp

instance Alpha UnyOp

data KeyValue a = KeyValue
  { key :: a,
    value :: a,
    hidden :: Bool
  }
  deriving
    ( Eq,
      Read,
      Show,
      Typeable,
      Generic,
      Generic1,
      Functor,
      Foldable,
      Traversable
    )

instance Alpha a => Alpha (KeyValue a)

instance Eq1 KeyValue where
  liftEq = liftEqDefault

instance Read1 KeyValue where
  liftReadsPrec = liftReadsPrecDefault

instance Show1 KeyValue where
  liftShowsPrec = liftShowsPrecDefault

data Object a = Object [KeyValue a]
  deriving
    ( Eq,
      Read,
      Show,
      Typeable,
      Generic,
      Generic1,
      Functor,
      Foldable,
      Traversable
    )

instance Alpha a => Alpha (Object a)

instance Eq1 Object where
  liftEq = liftEqDefault

instance Read1 Object where
  liftReadsPrec = liftReadsPrecDefault

instance Show1 Object where
  liftShowsPrec = liftShowsPrecDefault
