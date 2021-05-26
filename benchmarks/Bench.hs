{-# LANGUAGE QuasiQuotes #-}

import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Jsonnet
import Language.Jsonnet.Core (Core)
import Language.Jsonnet.Syntax.Annotated (Expr)
import Language.Jsonnet.TH.QQ
import Test.Tasty.Bench
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty)

render :: Pretty a => a -> LBS.ByteString
render = encodeUtf8 . pack . show . pretty

conf = Config ""

eval :: Expr -> IO LBS.ByteString
eval expr = do
  outp <- runJsonnetM conf (desugar expr >>= evaluate)
  pure (either render render outp)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "simple"
        [ bench "bench01" $ nfAppIO eval bench01,
          bench "bench02" $ nfAppIO eval bench02,
          bench "bench03" $ nfAppIO eval bench03
        ],
      bgroup
        "caching"
        [ bench "bench08" $ nfAppIO eval bench08
        ],
      bgroup
        "stdlib"
        [ bench "bench04" $ nfAppIO eval bench04
        ]
    ]

bench01 =
  [jsonnet|
    local sum(x) =
      if x == 0 then
      0
      else
      x + sum(x - 1);
    sum(300)
  |]

bench02 =
  [jsonnet|
    local Fib = {
      n: 1,
      local outer = self,
      r: if self.n <= 1
           then 1
           else (Fib { n: outer.n - 1 }).r + (Fib { n: outer.n - 2 }).r,
    };

    (Fib { n: 25 }).r
  |]

bench03 =
  [jsonnet|
    local fibonacci(n) =
      if n <= 1 then
        1
      else
        fibonacci(n - 1) + fibonacci(n - 2);
    fibonacci(25)
  |]

bench04 =
  [jsonnet|
    std.foldl(function(e, res) e + res, std.makeArray(20000, function(i) 'aaaaa'), '')
  |]

bench08 =
  [jsonnet|
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
