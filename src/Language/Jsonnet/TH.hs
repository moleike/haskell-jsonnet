{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module                  : Language.Jsonnet.TH
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.TH where

import Control.Monad.Except hiding (lift)
import Data.Binary (Binary, encode)
import Data.Data
import Data.Functor.Product
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Jsonnet.Annotate (forget)
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Desugar
import qualified Language.Jsonnet.Parser as Parser
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty ()
import Language.Jsonnet.Syntax
import Language.Jsonnet.Syntax.Annotated
import Text.PrettyPrint.ANSI.Leijen (pretty)

instance Data a => Lift (Arg a) where
  lift = liftData

instance Lift SrcSpan where
  lift = liftData

instance Lift Visibility where
  lift = liftData

instance Data a => Lift (Args a) where
  lift = liftData

instance Lift Strictness where
  lift = liftData

instance Lift Literal where
  lift = liftData

instance Lift Prim where
  lift = liftData

instance Lift BinOp where
  lift = liftData

instance Lift UnyOp where
  lift = liftData

instance Data a => Lift (EField a) where
  lift = liftData

instance Data a => Lift (Assert a) where
  lift = liftData

instance Data a => Lift (CompSpec a) where
  lift = liftData

instance Data a => Lift (ExprF a) where
  lift = liftData

instance
  ( Typeable a,
    Typeable f,
    Typeable g,
    Data (f a),
    Data (g a)
  ) =>
  Lift (Product f g a)
  where
  lift = liftData

instance Lift Expr where
  lift = liftData

liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

-- ouch: https://gitlab.haskell.org/ghc/ghc/-/issues/12596
liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (fmap liftText . cast)

parse0 :: FilePath -> Text -> Q Expr
parse0 path str = do
  parse' str >>= \case
    Left err -> fail (show $ pretty err)
    Right res -> pure res
  where
    parse' =
      runExceptT
        . ( Parser.parse path
              >=> Parser.resolveImports path
          )

parse :: FilePath -> Text -> Q Exp
parse path str =
  parse0 path str
    >>= liftDataWithText

-- | compiles a Jsonnet program down to a binary-encoded Core expression
--   with source spans stripped out. This is currently useful to preload
--   the std library and have a faster startup
compile :: FilePath -> Text -> Q Exp
compile path str =
  parse0 path str
    >>= lift
      . encode
      . desugar
      . forget
