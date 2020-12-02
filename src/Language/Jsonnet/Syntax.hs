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

type Param a = (Name, Maybe a)

data ExprF a
  = ELit Literal
  | EIdent Name
  | EFun [Param a] a
  | EApply a (Args a)
  | ELocal
      { bnds :: NonEmpty (Name, a),
        expr :: a
      }
  | EObj
     { locals :: [(Name, a)],
       fields :: [Field a]
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

mkFunF :: [Param a] -> a -> ExprF' a
mkFunF a = InL . EFun a

mkApplyF :: a -> Args a -> ExprF' a
mkApplyF a = InL . EApply a

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

mkObjectF :: [Field a] -> [(Name, a)] -> ExprF' a
mkObjectF fs ls = InL $ EObj ls fs

mkArrayF :: [a] -> ExprF' a
mkArrayF = InL . EArr

mkErrorF :: a -> ExprF' a
mkErrorF = InL . EErr

mkAssertF :: a -> Maybe a -> a -> ExprF' a
mkAssertF e m = InL . EAssert . Assert e m
