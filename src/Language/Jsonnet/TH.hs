{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module                  : Language.Jsonnet.TH
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.TH where

import Control.Monad ((>=>))
import Control.Monad.Except
import Data.Binary (encode)
import Data.Data
import Data.Text (Text)
import Data.Text qualified as T
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Jsonnet.Annotate (forget)
import Language.Jsonnet.Desugar
import Language.Jsonnet.Parser qualified as Parser
import Language.Jsonnet.Pretty (prettyError)
import Language.Jsonnet.Syntax.Annotated

liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

-- ouch: https://gitlab.haskell.org/ghc/ghc/-/issues/12596
liftDataWithText :: (Data a) => a -> Q Exp
liftDataWithText = dataToExpQ (fmap liftText . cast)

parse0 :: FilePath -> Text -> Q Expr
parse0 path str = do
  parse' str >>= \case
    Left err -> fail (show $ prettyError err)
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
