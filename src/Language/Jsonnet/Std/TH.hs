-- |
-- Module                  : Language.Jsonnet.Std.TH
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Std.TH where

import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import Language.Haskell.TH.Quote ()
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Jsonnet.TH (compile)
import TH.RelativePaths (pathRelativeToCabalPackage)

stdlibPath :: String
stdlibPath = "stdlib/std.jsonnet"

mkStdlib :: Q Exp
mkStdlib = do
  fp <- pathRelativeToCabalPackage stdlibPath
  addDependentFile fp
  src <- runIO (TIO.readFile stdlibPath)
  lbs <- compile stdlibPath src
  pure lbs
