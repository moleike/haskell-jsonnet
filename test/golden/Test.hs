{-# LANGUAGE OverloadedStrings #-}

-- |
module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Jsonnet (Config (..), jsonnet)
import Language.Jsonnet.Error
import Language.Jsonnet.Pretty ()
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty)

main :: IO ()
main = defaultMain =<< goldenTests

render :: Pretty a => a -> LBS.ByteString
render = encodeUtf8 . pack . show . pretty

run :: FilePath -> IO LBS.ByteString
run fp = do
  prog <- T.readFile fp
  outp <- jsonnet (Config fp) prog
  pure (either render render outp)

goldenTests :: IO TestTree
goldenTests = do
  jsonnetFiles <- findByExtension [".jsonnet"] "."
  return $
    testGroup
      "Jsonnet golden tests"
      [ goldenVsString
          (takeBaseName jsonnetFile) -- test name
          goldenFile -- golden file path
          (run jsonnetFile)
        | jsonnetFile <- jsonnetFiles,
          let goldenFile = replaceExtension jsonnetFile ".golden"
      ]
