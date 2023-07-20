{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module                  : Language.Jsonnet.Parser.SrcSpan
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Parser.SrcSpan where

import Data.Binary (Binary)
import Data.Data
import Data.Function (on)
import GHC.Generics (Generic)
import Text.Megaparsec.Pos (Pos, SourcePos (..))
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.TH

data SrcSpan = SrcSpan
  { spanBegin :: SourcePos,
    spanEnd :: SourcePos
  }
  deriving
    ( Ord,
      Eq,
      Show,
      Read,
      Generic,
      Typeable,
      Data
    )

$(makeClosedAlpha ''SrcSpan)

instance Binary SourcePos

instance Binary Pos

instance Binary SrcSpan

instance Subst b SourcePos => Subst b SrcSpan where
  subst _ _ = id
  substs _ = id

instance Semigroup SrcSpan where
  s1 <> s2 = SrcSpan ((min `on` spanBegin) s1 s2) ((max `on` spanEnd) s1 s2)

class HasSrcSpan a where
  srcSpan :: a -> SrcSpan

instance HasSrcSpan SrcSpan where
  srcSpan = id
