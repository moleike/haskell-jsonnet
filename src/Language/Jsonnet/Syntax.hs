{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Syntax where

import Control.Applicative (Const (..))
import Data.Functor.Sum
import Data.List.NonEmpty
import Data.Scientific (Scientific)
import qualified Data.Text as T
import GHC.Generics
import Language.Jsonnet.Common
import Text.Show.Deriving

type Name = String

data ExprF a
  = ELit Literal
  | EIdent Name
  | EFun [Name] a
  | EApply a [a]
  | ELocal
      { bnds :: NonEmpty (Name, a),
        expr :: a
      }
  | EObj (Object a)
  | EArr [a]
  | EErr a
  | ELookup a a
  | EIndex a a
  | EAssert
      { cond :: a,
        msg :: Maybe a,
        expr :: a
      }
  | EIf a a
  | EIfElse a a a
  | ESlice
      { expr :: a,
        start :: Maybe a,
        end :: Maybe a,
        step :: Maybe a
      }
  | EBinOp BinOp a a
  | EUnyOp UnyOp a
  deriving
    ( Show,
      Functor,
      Foldable,
      Traversable,
      Generic,
      Generic1
    )

deriveShow1 ''ExprF

newtype Import = Import FilePath
  deriving (Show, Eq)

type ExprF' = Sum ExprF (Const Import)

mkImportF :: String -> ExprF' a
mkImportF = InR . Const . Import

mkNullF :: ExprF' a
mkNullF = (InL . ELit) Null

mkIntF :: Integral b => b -> ExprF' a
mkIntF = InL . ELit . Number . fromIntegral

mkFloatF :: Scientific -> ExprF' a
mkFloatF = InL . ELit . Number

mkStrF :: String -> ExprF' a
mkStrF = InL . ELit . String . T.pack

mkBoolF :: Bool -> ExprF' a
mkBoolF = InL . ELit . Bool

mkIdentF :: Name -> ExprF' a
mkIdentF = InL . EIdent

mkFunF :: [Name] -> a -> ExprF' a
mkFunF b = InL . EFun b

mkApplyF :: a -> [a] -> ExprF' a
mkApplyF b = InL . EApply b

mkIfF :: a -> a -> ExprF' a
mkIfF c = InL . EIf c

mkIfElseF :: a -> a -> a -> ExprF' a
mkIfElseF c a = InL . EIfElse c a

mkLocalF :: NonEmpty (Name, a) -> a -> ExprF' a
mkLocalF n = InL . ELocal n

mkLookupF :: a -> a -> ExprF' a
mkLookupF e = InL . ELookup e

mkIndexF :: a -> a -> ExprF' a
mkIndexF e = InL . EIndex e

mkSliceF :: a -> Maybe a -> Maybe a -> Maybe a -> ExprF' a
mkSliceF e f g = InL . ESlice e f g

mkObjectF :: [KeyValue a] -> ExprF' a
mkObjectF = InL . EObj . Object

mkArrayF :: [a] -> ExprF' a
mkArrayF = InL . EArr

mkErrorF :: a -> ExprF' a
mkErrorF = InL . EErr

mkAssertF :: a -> Maybe a -> a -> ExprF' a
mkAssertF e m = InL . EAssert e m
