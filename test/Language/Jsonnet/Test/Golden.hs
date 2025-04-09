{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
module Language.Jsonnet.Test.Golden where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T (readFile)
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Jsonnet hiding (extVars)
import Language.Jsonnet.Pretty (ppJson, prettyError)
import Language.Jsonnet.Value
import Prettyprinter (Doc)
import System.FilePath (replaceExtension, takeBaseName)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

render :: (a -> Doc ann) -> a -> LBS.ByteString
render printer = encodeUtf8 . pack . show . printer

run :: Config -> IO LBS.ByteString
run conf = do
  prog <- T.readFile (fname conf)
  outp <- interpret conf prog
  pure (either (render prettyError) (render (ppJson 4)) outp)

-- Adapted from the C++ test suite
extVars :: ExtVars
extVars =
  ExtVars $
    M.fromList
      [ ("var1", VStr "test"),
        ("var2", unsafePerformIO $ interpretExtVar (ExtVar ExtCode (Inline "{x:1,y:2}")))
      ]

goldenTests :: IO TestTree
goldenTests = do
  jsonnetFiles <- findByExtension [".jsonnet"] "./test/golden"
  return $
    testGroup
      "Jsonnet golden tests"
      [ goldenVsString
          (takeBaseName jsonnetFile) -- test name
          goldenFile -- golden file path
          (run $ Config jsonnetFile extVars)
        | jsonnetFile <- jsonnetFiles,
          let goldenFile = replaceExtension jsonnetFile ".golden"
      ]
