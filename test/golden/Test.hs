{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
module Main where

import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Jsonnet
import Language.Jsonnet.Annotate
import Language.Jsonnet.Desugar
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Eval (mergeWith)
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Pretty ()
import qualified Language.Jsonnet.Std.Lib as Lib
import Language.Jsonnet.Std.TH (mkStdlib)
import Language.Jsonnet.Value
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty)

main :: IO ()
main = goldenTests >>= defaultMain

render :: Pretty a => a -> LBS.ByteString
render = encodeUtf8 . pack . show . pretty

run :: Config -> IO LBS.ByteString
run conf = do
  prog <- T.readFile (fname conf)
  outp <- interpret conf prog
  pure (either render render outp)

goldenTests :: IO TestTree
goldenTests = do
  jsonnetFiles <- findByExtension [".jsonnet"] "./test/golden"
  stdlib <- mkThunk std
  return $
    testGroup
      "Jsonnet golden tests"
      [ goldenVsString
          (takeBaseName jsonnetFile) -- test name
          goldenFile -- golden file path
          (run $ Config jsonnetFile stdlib)
        | jsonnetFile <- jsonnetFiles,
          let goldenFile = replaceExtension jsonnetFile ".golden"
      ]

-- the jsonnet stdlib is written in both jsonnet and Haskell, here we merge
-- the native (small, Haskell) with the interpreted (the splice mkStdlib)
std :: Eval Value
std = eval core >>= flip mergeObjects Lib.std
  where
    core = desugar (annMap (const ()) $mkStdlib)
    mergeObjects (VObj x) (VObj y) = pure $ VObj (x `mergeWith` y)
