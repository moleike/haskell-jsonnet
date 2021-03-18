{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Jsonnet.Std.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Jsonnet.TH (parse)
import qualified Data.Text.IO as TIO
import Language.Haskell.TH.Quote ()

stdlibPath :: String
stdlibPath = "stdlib/std.jsonnet"

mkStdlib :: Q Exp
mkStdlib  = do
  src <- runIO (TIO.readFile stdlibPath)
  ast <- parse stdlibPath src
  addDependentFile stdlibPath
  pure ast
