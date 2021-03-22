{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Std.TH where

import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import Language.Haskell.TH.Quote ()
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Jsonnet.TH (parse)

stdlibPath :: String
stdlibPath = "stdlib/std.jsonnet"

mkStdlib :: Q Exp
mkStdlib = do
  src <- runIO (TIO.readFile stdlibPath)
  ast <- parse stdlibPath src
  addDependentFile stdlibPath
  pure ast
