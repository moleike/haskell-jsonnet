{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module                  : Language.Jsonnet.Common
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Common where

import Data.Binary (Binary)
import Data.Data (Data)
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import Language.Jsonnet.Parser.SrcSpan (SrcSpan)

type Name a = Text

data Literal
    = Null
    | Bool Bool
    | String Text
    | Number Scientific
    deriving
        ( Show
        , Eq
        , Ord
        , Generic
        , Typeable
        , Data
        , Binary
        )

data Prim
    = UnyOp UnyOp
    | BinOp BinOp
    | Cond
    deriving
        ( Show
        , Eq
        , Generic
        , Typeable
        , Data
        , Binary
        )

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Lt
    | Le
    | Gt
    | Ge
    | Eq
    | Ne
    | And
    | Or
    | Xor
    | ShiftL
    | ShiftR
    | LAnd
    | LOr
    | In
    | Lookup
    deriving
        ( Show
        , Eq
        , Enum
        , Bounded
        , Generic
        , Typeable
        , Data
        , Binary
        )

data UnyOp
    = Compl
    | LNot
    | Plus
    | Minus
    | Err
    | Super
    deriving
        ( Eq
        , Show
        , Read
        , Enum
        , Bounded
        , Generic
        , Typeable
        , Data
        , Binary
        )

data Strictness = Strict | Lazy
    deriving
        ( Eq
        , Show
        , Read
        , Generic
        , Typeable
        , Data
        , Binary
        )

data Arg a = Pos a | Named String a
    deriving
        ( Eq
        , Read
        , Show
        , Typeable
        , Data
        , Generic
        , Generic1
        , Functor
        , Foldable
        , Traversable
        , Binary
        )
    deriving
        (Eq1, Show1)
        via FunctorClassesDefault Arg

data Args a = Args
    { args :: [Arg a]
    , strictness :: Strictness
    }
    deriving
        ( Eq
        , Read
        , Show
        , Typeable
        , Data
        , Generic
        , Generic1
        , Functor
        , Foldable
        , Traversable
        , Binary
        )
    deriving
        (Eq1, Show1)
        via FunctorClassesDefault Args

data Assert a = Assert
    { cond :: a
    , msg :: Maybe a
    }
    deriving
        ( Eq
        , Read
        , Show
        , Typeable
        , Data
        , Generic
        , Generic1
        , Functor
        , Foldable
        , Traversable
        )
    deriving
        (Eq1, Show1)
        via FunctorClassesDefault Assert

data CompSpec a = CompSpec
    { var :: String
    , forspec :: a
    , ifspec :: [a]
    }
    deriving
        ( Eq
        , Read
        , Show
        , Typeable
        , Data
        , Generic
        , Generic1
        , Functor
        , Foldable
        , Traversable
        )
    deriving
        (Eq1, Show1)
        via FunctorClassesDefault CompSpec

data StackFrame a = StackFrame
    { name :: Name a
    , span :: SrcSpan
    }
    deriving (Eq, Show)

newtype Backtrace a = Backtrace [StackFrame a]
    deriving (Eq, Show)

data Visibility = Visible | Hidden | Forced
    deriving
        ( Eq
        , Read
        , Show
        , Enum
        , Bounded
        , Generic
        , Typeable
        , Data
        , Binary
        )

instance Semigroup Visibility where
    a <> Visible = a
    _ <> b       = b

instance Monoid Visibility where
    mempty = Visible

data Import = Code | Str | Bin
    deriving (Show, Eq, Ord, Generic, Typeable, Data, Binary)
