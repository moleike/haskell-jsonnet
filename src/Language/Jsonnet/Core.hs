{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module                  : Language.Jsonnet.Core
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Core (
    Desugared,
    Core,
    Var (..),
    Param,
    RegularField (..),
    CField (..),
    Comp (..),
    pattern CLoc,
    pattern CLit,
    pattern CVar,
    pattern CLam,
    pattern CPrim,
    pattern CApp,
    pattern CLet,
    pattern CObj,
    pattern CArr,
    pattern CComp,
    pattern CImp,
    mkField,
    resolve,
) where

import Control.Applicative (asum)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Bifunctor.TH (deriveBifunctor)
import Data.Binary (Binary (..))
import Data.Fix (Fix (Fix), foldFix)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Jsonnet.Common (Args, Import, Literal, Name, Prim, Visibility)
import Language.Jsonnet.Parser.SrcSpan (SrcSpan)

type Param a = (Name, a)

data Var
    = Idx !Int !Int -- 2D de Bruijn
    | Self
    | Super
    | Std
    | Unbound !Name
    deriving stock (Show, Generic)
    deriving anyclass (Binary)

data RegularField a = RegularField
    { fieldKey :: a
    , fieldVal :: a
    , fieldVis :: Visibility
    }
    deriving stock
        (Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Binary)

data CField a
    = CRegularField (RegularField a)
    | -- | The desugared assertion expression
      CAssertField a
    deriving stock
        (Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Binary)

mkField :: a -> a -> Visibility -> CField a
mkField k v h = CRegularField (RegularField k v h)

data Comp a
    = ArrC Name (a, [a])
    | ObjC Name (CField a, [a])
    deriving stock
        (Show, Typeable, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Binary)

data CoreF v a
    = CLocF SrcSpan a
    | CLitF Literal
    | CVarF Name v
    | CLamF [Param a] a
    | CPrimF Prim
    | CAppF a (Args a)
    | CLetF [(Name, a)] a
    | CObjF [CField a]
    | CArrF [a]
    | CCompF (Comp a) a
    | CImpF Import FilePath
    deriving stock
        (Show, Typeable, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Binary)

$(deriveBifunctor ''CoreF)

type Desugared = Fix (CoreF Name)

type Core = Fix (CoreF Var)

instance Binary (Fix (CoreF Var)) where
    put (Fix expr) = put expr
    get = Fix <$> get

pattern CLoc :: SrcSpan -> Fix (CoreF v) -> Fix (CoreF v)
pattern CLoc sp e = Fix (CLocF sp e)

pattern CLit :: Literal -> Fix (CoreF v)
pattern CLit l = Fix (CLitF l)

pattern CVar :: Name -> v -> Fix (CoreF v)
pattern CVar n v = Fix (CVarF n v)

pattern CLam :: [Param (Fix (CoreF v))] -> Fix (CoreF v) -> Fix (CoreF v)
pattern CLam ps e = Fix (CLamF ps e)

pattern CPrim :: Prim -> Fix (CoreF v)
pattern CPrim p = Fix (CPrimF p)

pattern CApp :: Fix (CoreF v) -> Args (Fix (CoreF v)) -> Fix (CoreF v)
pattern CApp a es = Fix (CAppF a es)

pattern CLet :: [(Name, Fix (CoreF v))] -> Fix (CoreF v) -> Fix (CoreF v)
pattern CLet bnds e = Fix (CLetF bnds e)

pattern CObj :: [CField (Fix (CoreF v))] -> Fix (CoreF v)
pattern CObj fs = Fix (CObjF fs)

pattern CArr :: [Fix (CoreF v)] -> Fix (CoreF v)
pattern CArr es = Fix (CArrF es)

pattern CComp :: Comp (Fix (CoreF v)) -> Fix (CoreF v) -> Fix (CoreF v)
pattern CComp comp e = Fix (CCompF comp e)

pattern CImp :: Import -> FilePath -> Fix (CoreF v)
pattern CImp i fp = Fix (CImpF i fp)

-- data Params
--  = EmptyPs
--  | ConsPs (Rebind (Name Core, Embed (Maybe Core)) Params)
--  deriving (Show, Typeable, Generic)
-- instance Alpha Params

type Env = [[Name]]

resolve :: Desugared -> Core
resolve ast = foldFix alg ast []
  where
    alg :: CoreF Name (Env -> Core) -> (Env -> Core)
    alg = \case
        CVarF n v -> CVar n . (`resolveVar` v)
        CLetF bnds e -> \env ->
            let env' = map fst bnds : env
             in CLet (second ($ env') <$> bnds) (e env')
        CLamF ps e -> \env ->
            let env' = map fst ps : env
             in CLam (second ($ env') <$> ps) (e env')
        CCompF comp xs -> \env ->
            let n = case comp of
                    ArrC name _ -> name
                    ObjC name _ -> name
                env' = [n] : env
             in CComp (($ env') <$> comp) (xs env)
        -- reminaing constructors do not bind variables
        other -> \env -> Fix (first Unbound (fmap ($ env) other))

resolveVar :: Env -> Name -> Var
resolveVar env = \case
    "self" -> Self
    "super" -> Super
    n ->
        fromMaybe (unbound n) $
            asum $
                zipWith (\d row -> Idx d <$> elemIndex n row) [0 ..] env
  where
    unbound = \case
        "std" -> Std -- std is the only allowed free variable
        free -> Unbound free
