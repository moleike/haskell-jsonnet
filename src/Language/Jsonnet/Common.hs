{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Common where

import Data.Data (Data)
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Scientific (Scientific)
import Data.String
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import Language.Jsonnet.Parser.SrcSpan
import Text.Show.Deriving
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.TH (makeClosedAlpha)

data Literal
  = Null
  | Bool Bool
  | String Text
  | Number Scientific
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

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
  deriving (Show, Eq, Generic, Typeable, Data)

data UnyOp
  = Compl
  | LNot
  | Plus
  | Minus
  deriving (Show, Eq, Enum, Bounded, Generic, Typeable, Data)

data ArithOp = Add | Sub | Mul | Div | Mod
  deriving (Show, Eq, Enum, Bounded, Generic, Typeable, Data)

data CompOp = Lt | Le | Gt | Ge | Eq | Ne
  deriving (Show, Eq, Enum, Bounded, Generic, Typeable, Data)

data BitwiseOp = And | Or | Xor | ShiftL | ShiftR
  deriving (Show, Eq, Enum, Bounded, Generic, Typeable, Data)

data LogicalOp = LAnd | LOr
  deriving (Show, Eq, Enum, Bounded, Generic, Typeable, Data)

instance Alpha ArithOp

instance Alpha BinOp

instance Alpha CompOp

instance Alpha BitwiseOp

instance Alpha LogicalOp

instance Alpha UnyOp

data Strictness = Strict | Lazy
  deriving (Eq, Read, Show, Generic, Typeable, Data)

instance Alpha Strictness

data Arg a = Pos a | Named String a
  deriving
    ( Eq,
      Read,
      Show,
      Typeable,
      Data,
      Generic,
      Generic1,
      Functor,
      Foldable,
      Traversable
    )

deriveShow1 ''Arg

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
      Data,
      Generic,
      Functor,
      Foldable,
      Traversable
    )

deriveShow1 ''Args

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
      Data,
      Generic,
      Functor,
      Foldable,
      Traversable
    )

instance Alpha a => Alpha (Assert a)

deriveShow1 ''Assert

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
      Data,
      Generic,
      Functor,
      Foldable,
      Traversable
    )

deriveShow1 ''CompSpec

instance Alpha a => Alpha (CompSpec a)

data StackFrame a = StackFrame
  { name :: Maybe (Name a),
    span :: SrcSpan
  }
  deriving (Eq, Show)

pushStackFrame ::
  StackFrame a ->
  Backtrace a ->
  Backtrace a
pushStackFrame x (Backtrace xs) = Backtrace (x : xs)

data Backtrace a = Backtrace [StackFrame a]
  deriving (Eq, Show)

data Visibility = Visible | Hidden | Forced
  deriving
    ( Eq,
      Read,
      Show,
      Generic,
      Typeable,
      Data
    )

instance Alpha Visibility

class HasVisibility a where
  visible :: a -> Bool
  forced :: a -> Bool
  hidden :: a -> Bool

data Hideable a = Hideable a Visibility
  deriving
    ( Eq,
      Read,
      Show,
      Generic,
      Functor,
      Typeable,
      Data
    )

instance Alpha a => Alpha (Hideable a)

instance HasVisibility (Hideable a) where
  visible (Hideable _ Visible) = True
  visible _ = False

  forced (Hideable _ Forced) = True
  forced _ = False

  hidden (Hideable _ Hidden) = True
  hidden _ = False
