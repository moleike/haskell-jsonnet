{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module                  : Language.Jsonnet.Syntax
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Syntax where

import Control.Applicative (Const (..))
import Data.Data (Data)
import Data.Eq.Deriving
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Functor.Sum
import Data.List.NonEmpty
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics
import Language.Jsonnet.Common
import Unbound.Generics.LocallyNameless

type Ident = String

type Param a = (Ident, Maybe a)

data EField a = EField
  { key :: a,
    value :: a,
    computed :: Bool,
    visibility :: Visibility,
    override :: Bool
  }
  deriving stock
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
  deriving anyclass (Alpha)
  deriving
    (Eq1, Show1)
    via FunctorClassesDefault EField

data ExprF a
  = ENull
  | EBool Bool
  | ENum Scientific
  | EStr Text Quoting
  | EIdent Ident
  | EFun [Param a] a
  | EApply a (Args a)
  | ELocal
      { -- | non-empty list of recursive bindings
        bnds :: NonEmpty (Ident, a),
        -- | body expression
        expr :: a
      }
  | EObj
      { -- |
        locals :: [(Ident, a)],
        -- |
        fields :: [EField a]
        --asserts :: [Assert a]
      }
  | EArr [a]
  | EErr a
  | ELookup a a
  | EIndex a a
  | EAssert (Assert a)
  | EIf a a
  | EIfElse a a a
  | ESlice
      { -- |
        expr :: a,
        -- |
        start :: Maybe a,
        -- |
        end :: Maybe a,
        -- |
        step :: Maybe a
      }
  | EBinOp BinOp a a
  | EUnyOp UnyOp a
  | EArrComp
      { -- |
        expr :: a,
        -- |
        comp :: NonEmpty (CompSpec a)
      }
  | EObjComp
      { -- |
        field :: EField a,
        -- |
        locals :: [(Ident, a)],
        -- |
        comp :: NonEmpty (CompSpec a)
      }
  deriving stock
    ( Eq,
      Show,
      Functor,
      Foldable,
      Traversable,
      Generic,
      Generic1,
      Typeable,
      Data
    )
  deriving
    (Eq1, Show1)
    via FunctorClassesDefault ExprF

newtype Import = Import FilePath
  deriving (Show, Eq)

type ExprF' = Sum ExprF (Const Import)

mkImportF :: String -> ExprF' a
mkImportF = InR . Const . Import

mkNullF :: ExprF' a
mkNullF = InL ENull

mkIntF :: Integral b => b -> ExprF' a
mkIntF = InL . ENum . fromIntegral

mkFloatF :: Scientific -> ExprF' a
mkFloatF = InL . ENum

mkTextF :: Text -> Quoting -> ExprF' a
mkTextF s = InL . EStr s

mkStrF :: String -> Quoting -> ExprF' a
mkStrF s = mkTextF (T.pack s)

mkBoolF :: Bool -> ExprF' a
mkBoolF = InL . EBool

mkIdentF :: Ident -> ExprF' a
mkIdentF = InL . EIdent

mkFunF :: [Param a] -> a -> ExprF' a
mkFunF a = InL . EFun a

mkApplyF :: a -> Args a -> ExprF' a
mkApplyF a = InL . EApply a

mkIfF :: a -> a -> ExprF' a
mkIfF c = InL . EIf c

mkIfElseF :: a -> a -> a -> ExprF' a
mkIfElseF c a = InL . EIfElse c a

mkLocalF :: NonEmpty (Ident, a) -> a -> ExprF' a
mkLocalF n = InL . ELocal n

mkLookupF :: a -> a -> ExprF' a
mkLookupF e = InL . ELookup e

mkIndexF :: a -> a -> ExprF' a
mkIndexF e = InL . EIndex e

mkSliceF ::
  -- |
  a ->
  -- |
  Maybe a ->
  -- |
  Maybe a ->
  -- |
  Maybe a ->
  ExprF' a
mkSliceF e f g = InL . ESlice e f g

mkObjectF ::
  -- |
  [EField a] ->
  -- |
  [(Ident, a)] ->
  ExprF' a
mkObjectF fs ls = InL $ EObj ls fs

mkArrayF :: [a] -> ExprF' a
mkArrayF = InL . EArr

mkErrorF :: a -> ExprF' a
mkErrorF = InL . EErr

mkAssertF :: a -> Maybe a -> a -> ExprF' a
mkAssertF e m = InL . EAssert . Assert e m

mkArrCompF :: a -> NonEmpty (CompSpec a) -> ExprF' a
mkArrCompF e = InL . EArrComp e

mkObjCompF ::
  -- |
  EField a ->
  -- |
  [(Ident, a)] ->
  -- |
  NonEmpty (CompSpec a) ->
  ExprF' a
mkObjCompF f ls = InL . EObjComp f ls
