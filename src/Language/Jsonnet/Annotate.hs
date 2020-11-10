{-# LANGUAGE PatternSynonyms #-}

module Language.Jsonnet.Annotate where

import Control.Applicative (Const (..))
import Data.Fix
import Data.Functor.Product

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

inherit :: Functor f => (Fix f -> a -> (b, a)) -> a -> Fix f -> Ann f b
inherit h root = go root
  where
    go p s@(Fix t) =
      let (b, a) =
            h s p
       in Fix (AnnF (fmap (go a) t) b)

annZip :: Functor f => Fix (AnnF (AnnF f a) b) -> Ann f (a, b)
annZip (Fix (AnnF (AnnF t x) y)) = Fix (AnnF (fmap annZip t) (x, y))

--instance (Show a, Show1 f) => Show1 (Const a :*: f) where
--  liftShowsPrec = liftShowsPrecDefault
