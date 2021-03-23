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
import Language.Jsonnet.Error
import Language.Jsonnet.Pretty ()
import Language.Jsonnet.Error
import Language.Jsonnet.Annotate
import Language.Jsonnet.Desugar
import Language.Jsonnet.Eval
import Language.Jsonnet.Eval (mergeWith)
import Language.Jsonnet.Eval.Monad
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty)
import qualified Language.Jsonnet.Std.Lib as Lib
import Language.Jsonnet.Std.TH (mkStdlib)
import Language.Jsonnet.Value

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
  runExceptT std >>= \case
    Left err -> error (show $ pretty err)
    Right stdlib -> do
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
std :: ExceptT Error IO Value
std = do
  ast <- pure $(mkStdlib)
  core <- pure $ desugar (annMap (const ()) ast)
  runEval emptyEnv (eval core >>= flip mergeObjects Lib.std)
  where
    mergeObjects (VObj x) (VObj y) = pure $ VObj (x `mergeWith` y)
