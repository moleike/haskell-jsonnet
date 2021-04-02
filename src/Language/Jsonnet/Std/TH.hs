{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Std.TH where

import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import Language.Haskell.TH.Quote ()
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Jsonnet.TH (parse)
import TH.RelativePaths (pathRelativeToCabalPackage)

stdlibPath :: String
stdlibPath = "stdlib/std.jsonnet"

mkStdlib :: Q Exp
mkStdlib = do
  fp <- pathRelativeToCabalPackage stdlibPath
  addDependentFile fp
  src <- runIO (TIO.readFile stdlibPath)
  ast <- parse stdlibPath src
  pure ast
