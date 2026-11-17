{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module                  : Language.Jsonnet.Core
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Core where

import Data.Binary (Binary (..))
import Data.Fix (Fix (Fix))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Jsonnet.Common (Args, Import, Literal, Name, Prim, Visibility)
import Language.Jsonnet.Parser.SrcSpan (SrcSpan)

type Param a = (Name Core, a)

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
    = ArrC (Name a) (a, [a])
    | ObjC (Name a) (CField a, [a])
    deriving stock
        (Show, Typeable, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Binary)

data CoreF a
    = CLocF SrcSpan a
    | CLitF Literal
    | CVarF (Name a)
    | CLamF [Param a] a
    | CPrimF Prim
    | CAppF a (Args a)
    | CLetF [(Name a, a)] a
    | CObjF [CField a]
    | CArrF [a]
    | CCompF (Comp a) a
    | CImpF Import FilePath
    deriving stock
        (Show, Typeable, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Binary)

type Core = Fix CoreF

instance Binary (Fix CoreF) where
  put (Fix expr) = put expr
  get = Fix <$> get

pattern CLoc :: SrcSpan -> Core -> Core
pattern CLoc sp e = Fix (CLocF sp e)

pattern CLit :: Literal -> Core
pattern CLit l = Fix (CLitF l)

pattern CVar :: Name Core -> Core
pattern CVar n = Fix (CVarF n)

pattern CLam :: [Param Core] -> Core -> Core
pattern CLam ps e = Fix (CLamF ps e)

pattern CPrim :: Prim -> Core
pattern CPrim p = Fix (CPrimF p)

pattern CApp :: Core -> Args Core -> Core
pattern CApp a es = Fix (CAppF a es)

pattern CLet :: [(Name Core, Core)] -> Core -> Core
pattern CLet bnds e = Fix (CLetF bnds e)

pattern CObj :: [CField Core] -> Core
pattern CObj fs = Fix (CObjF fs)

pattern CArr :: [Core] -> Core
pattern CArr es = Fix (CArrF es)

pattern CComp :: Comp Core -> Core -> Core
pattern CComp comp e = Fix (CCompF comp e)

pattern CImp :: Import -> FilePath -> Core
pattern CImp i fp = Fix (CImpF i fp)

-- data Params
--  = EmptyPs
--  | ConsPs (Rebind (Name Core, Embed (Maybe Core)) Params)
--  deriving (Show, Typeable, Generic)
-- instance Alpha Params
