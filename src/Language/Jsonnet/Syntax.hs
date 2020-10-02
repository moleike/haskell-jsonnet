{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Syntax where

import Control.Applicative (Const (..))
import Data.Functor.Sum
import Data.List.NonEmpty
import qualified Data.Text as T
import GHC.Generics
import Language.Jsonnet.Common
import Text.Show.Deriving

type Name = String

data ExprF a
  = ELit Literal
  | EIdent Name
  | EFun (NonEmpty Name) a
  | EApply a (NonEmpty a)
  | ELocal (NonEmpty (Name, a)) a
  | EObj (Object a)
  | EArr [a]
  | EBinOp BinOp a a
  | EUnyOp UnyOp a
  | EIf a a
  | EIfElse a a a
  | EErr a
  | ELookup a a
  | EAssert a (Maybe a) a
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

mkFloatF :: Double -> ExprF' a
mkFloatF = InL . ELit . Number

mkStrF :: String -> ExprF' a
mkStrF = InL . ELit . String . T.pack

mkBoolF :: Bool -> ExprF' a
mkBoolF = InL . ELit . Bool

mkIdentF :: Name -> ExprF' a
mkIdentF = InL . EIdent

mkFunF :: NonEmpty Name -> a -> ExprF' a
mkFunF b = InL . EFun b

mkApplyF :: a -> NonEmpty a -> ExprF' a
mkApplyF b = InL . EApply b

mkIfF :: a -> a -> ExprF' a
mkIfF c = InL . EIf c

mkIfElseF :: a -> a -> a -> ExprF' a
mkIfElseF c a = InL . EIfElse c a

mkLocalF :: NonEmpty (Name, a) -> a -> ExprF' a
mkLocalF n = InL . ELocal n

mkLookupF :: a -> a -> ExprF' a
mkLookupF e = InL . ELookup e

mkObjectF :: [KeyValue a] -> ExprF' a
mkObjectF = InL . EObj . Object

mkArrayF :: [a] -> ExprF' a
mkArrayF = InL . EArr

mkErrorF :: a -> ExprF' a
mkErrorF = InL . EErr

mkAssertF :: a -> Maybe a -> a -> ExprF' a
mkAssertF e m = InL . EAssert e m
