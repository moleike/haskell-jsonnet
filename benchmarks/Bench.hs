{-# LANGUAGE QuasiQuotes #-}

import Test.Tasty.Bench
import Language.Jsonnet.TH.QQ
import Language.Jsonnet
import Language.Jsonnet.Core (Core)
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy
import Language.Jsonnet.Syntax.Annotated (Expr)

bench01 = [jsonnet|
  local sum(x) =
    if x == 0 then
    0
    else
    x + sum(x - 1);
  sum(300)
|]

bench03 = [jsonnet|
  local fibonacci(n) =
    if n <= 1 then
      1
    else
      fibonacci(n - 1) + fibonacci(n - 2);
  fibonacci(25)
|]

bench08 = [jsonnet|
  local fibnext = {
    a: super.a + super.b,
    b: super.a,
  };
  local fib(n) =
    if n == 0 then
      { a: 1, b: 1 }
    else
      fib(n - 1) + fibnext;

  fib(25)
|]

render :: Pretty a => a -> LBS.ByteString
render = encodeUtf8 . pack . show . pretty

conf = Config ""

eval :: Expr -> IO LBS.ByteString
eval expr = do
  outp <- runJsonnetM conf (desugar expr >>= evaluate)
  pure (either render render outp)

main :: IO ()
main = defaultMain
  [ bgroup "simple"
    [ bench "bench01" $ nfAppIO eval bench01
    , bench "bench03" $ nfAppIO eval bench03
    ]
  , bgroup "caching"
    [ bench "bench08" $ nfAppIO eval bench08
    ]
  ]
