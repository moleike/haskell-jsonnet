-- |
module Main where

import Language.Jsonnet.Test.Golden
import Language.Jsonnet.Test.Roundtrip
import Test.Tasty

main :: IO ()
main = do
  goldenTests' <- goldenTests
  defaultMain $
    testGroup
      "tests"
      [ testRoundtripGroup,
        goldenTests'
      ]
