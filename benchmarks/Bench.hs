{-# LANGUAGE QuasiQuotes #-}

import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy hiding (show)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Jsonnet
import Language.Jsonnet.Desugar (desugar)
import Language.Jsonnet.Pretty (ppJsonIndent, prettyError)
import Language.Jsonnet.Syntax.Annotated (Expr)
import Language.Jsonnet.TH.QQ
import Prettyprinter (Doc)
import Test.Tasty.Bench

render :: (a -> Doc ann) -> a -> LBS.ByteString
render printer = encodeUtf8 . pack . show . printer

eval :: Expr -> IO LBS.ByteString
eval expr = do
    outp <- runJsonnetM defaultConfig (evaluate $ desugar expr)
    pure (either (render prettyError) (render (ppJsonIndent 4)) outp)

main :: IO ()
main =
    defaultMain
        [ bgroup
            "closures-and-eval-loop"
            [ -- closure allocation and AST traversal
              bench "function-factory" $ nfAppIO eval benchClosures
            , bench "currying" $ nfAppIO eval benchCurrying
            , bench "deep-inheritance" $ nfAppIO eval benchDeepInheritance
            , bench "fib-oop" $ nfAppIO eval bench02
            ]
        , bgroup
            "comprehensions"
            [ -- comprehension loop
              bench "array-comprehension" $ nfAppIO eval benchArrayComp
            , bench "computed-fields" $ nfAppIO eval benchComputedFields
            ]
        ]

benchClosures :: Expr
benchClosures =
    [jsonnet|
    local adder(n) = function(x) x + n;
    local fns = std.makeArray(300, function(i) adder(i));
    std.foldl(function(acc, f) acc + f(acc), fns, 0)
  |]

benchCurrying :: Expr
benchCurrying =
    [jsonnet|
    local curry2(f) = function(a) function(b) f(a, b);
    local add = curry2(function(a, b) a + b);
    local mul = curry2(function(a, b) a * b);
    local xs = std.range(1, 200);
    std.foldl(function(acc, x) add(acc)(mul(x)(x)), xs, 0)
  |]

benchDeepInheritance :: Expr
benchDeepInheritance =
    [jsonnet|
    local chain(n) =
      if n <= 0 then { value: 0, depth: 0 }
      else chain(n - 1) { value: super.value + n, depth: super.depth + 1 };
    local result = chain(150);
    result.value
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

benchArrayComp :: Expr
benchArrayComp =
    [jsonnet|
    local n = 60;
    local pairs = [ [i, j] for i in std.range(1, n) for j in std.range(i, n) ];
    std.length(pairs)
  |]

benchComputedFields :: Expr
benchComputedFields =
    [jsonnet|
    local n = 300;
    local obj = { ['key' + i]: i * i for i in std.range(0, n) };
    std.foldl(function(acc, k) acc + obj[k], std.objectFields(obj), 0)
  |]
