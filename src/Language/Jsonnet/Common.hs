{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Common where

import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Scientific (Scientific)
import Data.String
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import Language.Jsonnet.Parser.SrcSpan
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.TH (makeClosedAlpha)

data Literal
  = Null
  | Bool Bool
  | String Text
  | Number Scientific
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
  | In
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

data Strictness = Strict | Lazy
  deriving (Eq, Read, Show, Generic)

instance Alpha Strictness

data Arg a = Pos a | Named String a
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

instance Alpha a => Alpha (Arg a)

data Args a = Args
  { args :: [Arg a],
    strictness :: Strictness
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

instance Alpha a => Alpha (Args a)

data Assert a = Assert
  { cond :: a,
    msg :: Maybe a,
    expr :: a
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

instance Alpha a => Alpha (Assert a)

instance Eq1 Assert where
  liftEq = liftEqDefault

instance Read1 Assert where
  liftReadsPrec = liftReadsPrecDefault

instance Show1 Assert where
  liftShowsPrec = liftShowsPrecDefault

data CompSpec a = CompSpec
  { var :: String,
    forspec :: a,
    ifspec :: Maybe a
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

instance Alpha a => Alpha (CompSpec a)

instance Eq1 CompSpec where
  liftEq = liftEqDefault

instance Read1 CompSpec where
  liftReadsPrec = liftReadsPrecDefault

instance Show1 CompSpec where
  liftShowsPrec = liftShowsPrecDefault

-- this is just my current workaround to report
-- better error messages; we collect the source spans
-- at call site, but reporting a bunch of locations
-- doesn't seem very heplful either
type CallStack = [SrcSpan]

data Backtrace = Backtrace (Maybe CallStack)
  deriving (Show)
