{-# LANGUAGE OverloadedStrings #-}

module Language.Jsonnet.Test.JPath (jpathTests) where

import Data.Text.IO qualified as T
import Language.Jsonnet
import Language.Jsonnet.Pretty
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

run :: Config -> IO String
run conf = do
    prog <- T.readFile (fname conf)
    outp <- interpret conf prog
    pure (either show (show . ppJson 4) outp)

jpathTests :: TestTree
jpathTests =
    testGroup
        "JPath Import Resolution"
        [ testCase "Local directory beats JPath" $
            withSystemTempDirectory "jsonnet" $ \tmp -> do
                writeFile (tmp </> "main.jsonnet") "import 'target.jsonnet'"
                writeFile (tmp </> "target.jsonnet") "\"local\""

                createDirectory (tmp </> "lib")
                writeFile (tmp </> "lib" </> "target.jsonnet") "\"jpath\""

                let conf = defaultConfig{fname = tmp </> "main.jsonnet", jpaths = [tmp </> "lib"]}
                result <- run conf
                result @?= "\"local\""
        , testCase "Falls back to JPath when not in local directory" $
            withSystemTempDirectory "jsonnet" $ \tmp -> do
                writeFile (tmp </> "main.jsonnet") "import 'target.jsonnet'"

                createDirectory (tmp </> "lib")
                writeFile (tmp </> "lib" </> "target.jsonnet") "\"jpath\""

                let conf = defaultConfig{fname = tmp </> "main.jsonnet", jpaths = [tmp </> "lib"]}
                result <- run conf
                result @?= "\"jpath\""
        , testCase "Right-most CLI arg (left-most in list) wins" $
            withSystemTempDirectory "jsonnet" $ \tmp -> do
                writeFile (tmp </> "main.jsonnet") "import 'target.jsonnet'"

                createDirectory (tmp </> "lib1")
                writeFile (tmp </> "lib1" </> "target.jsonnet") "\"lib1\""

                createDirectory (tmp </> "lib2")
                writeFile (tmp </> "lib2" </> "target.jsonnet") "\"lib2\""

                -- lib2 is listed first, simulating the right-most CLI argument
                let conf = defaultConfig{fname = tmp </> "main.jsonnet", jpaths = [tmp </> "lib2", tmp </> "lib1"]}
                result <- run conf
                result @?= "\"lib2\""
        ]
