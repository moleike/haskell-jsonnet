{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module                  : Language.Jsonnet.Annotate
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
--
-- Annotated trees, based on fixplate
module Language.Jsonnet.Annotate where

import Control.Applicative (Const (..))
import Data.Fix
import Data.Functor.Product

type AnnF f a = Product (Const a) f

-- | Annotated fixed-point type. Equivalent to CoFree f a
type Ann f a = Fix (AnnF f a)

pattern AnnF f a = Pair (Const a) f

annMap :: Functor f => (a -> b) -> Ann f a -> Ann f b
annMap g = go
  where
    go (Fix (AnnF f a)) = Fix $ AnnF (fmap go f) (g a)

forget :: Functor f => Ann f a -> Ann f ()
forget = annMap (const ())

--forget :: Functor f => Ann f a -> Fix f
--forget (Fix (AnnF f _)) = Fix $ fmap forget f

attrib :: Ann f a -> a
attrib (Fix (AnnF _ a)) = a

inherit :: Functor f => (Fix f -> a -> (b, a)) -> a -> Fix f -> Ann f b
inherit h = go
  where
    go p s@(Fix t) =
      let (b, a) =
            h s p
       in Fix (AnnF (fmap (go a) t) b)

annZip :: Functor f => Fix (AnnF (AnnF f a) b) -> Ann f (a, b)
annZip (Fix (AnnF (AnnF t x) y)) = Fix (AnnF (fmap annZip t) (x, y))

--instance (Show a, Show1 f) => Show1 (Const a :*: f) where
--  liftShowsPrec = liftShowsPrecDefault
