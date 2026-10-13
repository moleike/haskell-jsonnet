-- |
-- Module                  : Language.Jsonnet.Compiler
-- Copyright               : (c) 2020-2026 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Compiler (
    compileModule,
) where

import Control.Monad.Except (ExceptT)
import Data.Text (Text)

import Language.Jsonnet.Check (check)
import Language.Jsonnet.Core (Core)
import Language.Jsonnet.Desugar
import Language.Jsonnet.Error
import Language.Jsonnet.Parser (parse)

compileModule :: FilePath -> Text -> ExceptT Error IO Core
compileModule path content = do
    expr <- parse path content
    check expr
    pure (desugar expr)
