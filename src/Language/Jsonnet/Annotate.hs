{-# LANGUAGE PatternSynonyms #-}

module Language.Jsonnet.Annotate where

import Control.Applicative (Const (..))
import Data.Fix
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Functor.Product
import Data.List.NonEmpty as NE

-- | Annotated trees, based on fixplate
type AnnF f a = Product (Const a) f

type Ann f a = Fix (AnnF f a)

pattern AnnF f a = Pair (Const a) f

annMap :: Functor f => (a -> b) -> Ann f a -> Ann f b
annMap g = go
  where
    go (Fix (AnnF f a)) = Fix $ AnnF (fmap go f) (g a)

forget :: Functor f => Ann f a -> Fix f
forget (Fix (AnnF f _)) = Fix $ fmap forget f

attrib :: Ann f a -> a
attrib (Fix (AnnF _ a)) = a

synthetise :: Functor f => (f a -> a) -> Fix f -> Ann f a
synthetise g = go
  where
    go (Fix f) = Fix $ AnnF x (h x)
      where
        x = fmap go f
        h = g . fmap attrib

--instance (Show a, Show1 f) => Show1 (Const a :*: f) where
--  liftShowsPrec = liftShowsPrecDefault
