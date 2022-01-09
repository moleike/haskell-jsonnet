-- |
module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Jsonnet.Test.Golden
import Language.Jsonnet.Test.Roundtrip
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Hedgehog

main :: IO ()
main = do
  goldenTests' <- goldenTests
  defaultMain $
    testGroup
      "tests"
      -- Roundtrip test is temporarily commented out, see:
      -- https://github.com/moleike/haskell-jsonnet/issues/45
      [ -- testRoundtripGroup,
        goldenTests'
      ]
