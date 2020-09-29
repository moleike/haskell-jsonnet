{-# LANGUAGE OverloadedStrings #-}

-- |
module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.Text.Lazy (fromStrict, pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Jsonnet
import Language.Jsonnet.Pretty ()
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Text.PrettyPrint.ANSI.Leijen (pretty)

main :: IO ()
main = defaultMain =<< goldenTests

run :: FilePath -> IO LBS.ByteString
run fp = do
  prog <- T.readFile fp
  outp <- jsonnet (Config fp) prog
  case outp of
    Left err ->
      pure $ encodeUtf8 $ pack $ show $ pretty err
    Right result ->
      pure $ encodeUtf8 $ fromStrict result

goldenTests :: IO TestTree
goldenTests = do
  jsonnetFiles <- findByExtension [".jsonnet"] "."
  return $
    testGroup
      "Jsonnet golden tests"
      [ goldenVsString
          (takeBaseName jsonnetFile) -- test name
          jsonFile -- golden file path
          (run jsonnetFile)
        | jsonnetFile <- jsonnetFiles,
          let jsonFile = replaceExtension jsonnetFile ".json"
      ]
