{-# LANGUAGE QuasiQuotes #-}

import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Jsonnet
import Language.Jsonnet.Pretty (ppJson, prettyError)
import Language.Jsonnet.Syntax.Annotated (Expr)
import Language.Jsonnet.TH.QQ
import Prettyprinter (Doc)
import Test.Tasty.Bench

render :: (a -> Doc ann) -> a -> LBS.ByteString
render printer = encodeUtf8 . pack . show . printer

conf :: Config
conf = Config "" mempty

eval :: Expr -> IO LBS.ByteString
eval expr = do
  outp <- runJsonnetM conf (desugar expr >>= evaluate)
  pure (either (render prettyError) (render (ppJson 4)) outp)

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
        [ bench "bench04" $ nfAppIO eval bench04,
          bench "bench06" $ nfAppIO eval bench06
        ]
    ]

bench01 :: Expr
bench01 =
  [jsonnet|
    local sum(x) =
      if x == 0 then
      0
      else
      x + sum(x - 1);
    sum(300)
  |]

bench02 :: Expr
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

bench03 :: Expr
bench03 =
  [jsonnet|
    local fibonacci(n) =
      if n <= 1 then
        1
      else
        fibonacci(n - 1) + fibonacci(n - 2);
    fibonacci(25)
  |]

bench04 :: Expr
bench04 =
  [jsonnet|
    std.foldl(function(e, res) e + res, std.makeArray(20000, function(i) 'aaaaa'), '')
  |]

bench06 :: Expr
bench06 =
  [jsonnet|
    // A benchmark for builtin sort

    local reverse = std.reverse;
    local sort = std.sort;

    true
    && std.assertEqual(std.range(1, 500), sort(std.range(1, 500)))
    && std.assertEqual(std.range(1, 1000), sort(std.range(1, 1000)))
    && std.assertEqual(reverse(std.range(1, 1000)), sort(std.range(1, 1000), keyF=function(x) -x))
    && std.assertEqual(std.range(1, 1000), sort(reverse(std.range(1, 1000))))
    && std.assertEqual(std.makeArray(2000, function(i) std.floor((i + 2) / 2)), sort(std.range(1, 1000) + reverse(std.range(1, 1000))))
  |]

bench08 :: Expr
bench08 =
  [jsonnet|
    local fib(n) =
      local go(n) =
        if n <= 1 then
          { ['fib0']: 1, ['fib1']: 1 }
        else
          go(n - 1) {
            ['fib'+n]: super['fib'+(n-1)] + super['fib'+(n-2)]
          };

      go(n)['fib'+n];

    fib(25)
  |]
