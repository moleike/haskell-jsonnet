{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
module Language.Jsonnet.Parser.SrcSpan where

import Data.Data
import Data.Function (on)
import GHC.Generics (Generic)
import Text.Megaparsec.Pos (SourcePos (..))
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

instance Subst b SrcSpan where
  subst _ _ = id
  substs _ = id

instance Semigroup SrcSpan where
  s1 <> s2 = SrcSpan ((min `on` spanBegin) s1 s2) ((max `on` spanEnd) s1 s2)

class HasSrcSpan a where
  srcSpan :: a -> SrcSpan

instance HasSrcSpan SrcSpan where
  srcSpan = id
