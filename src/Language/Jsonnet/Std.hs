{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Jsonnet.Std (std) where

import Control.Applicative
import Control.Monad.Except
import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import Data.Word
import Language.Jsonnet.Error
import Language.Jsonnet.Eval as E
import Language.Jsonnet.Pretty ()
import Numeric
import Text.Printf

-- The Jsonnet standard library, `std`, with each builtin function implemented
-- in Haskell code (incomplete)

std :: Value
std = VObj $ (Thunk . pure) <$> H.fromList xs
  where
    xs :: [(Text, Value)]
    xs =
      [ ("assertEqual", inj E.assertEqual),
        ("type", inj E.valueType),
        ("isString", inj (isType "string")),
        ("isBoolean", inj (isType "boolean")),
        ("isNumber", inj (isType "number")),
        ("isObject", inj (isType "object")),
        ("isArray", inj (isType "array")),
        ("isFunction", inj (isType "function")),
        ("objectFields", inj (H.keys @Text @Value)),
        ("length", inj length'),
        ("abs", inj (abs @Double)),
        ("sign", inj (signum @Double)), -- incl. 0.0, (-0.0), and NaN
        ("max", inj (max @Double)),
        ("min", inj (min @Double)),
        ("pow", inj ((^^) @Double @Int)),
        ("exp", inj (exp @Double)),
        ("log", inj (log @Double)),
        ("exponent", inj (exponent @Double)),
        ("mantissa", inj (significand @Double)),
        ("floor", inj (floor @Double @Integer)),
        ("ceil", inj (ceiling @Double @Integer)),
        ("sqrt", inj (sqrt @Double)),
        ("sin", inj (sin @Double)),
        ("cos", inj (cos @Double)),
        ("tan", inj (tan @Double)),
        ("asin", inj (asin @Double)),
        ("acos", inj (acos @Double)),
        ("atan", inj (atan @Double)),
        ("mod", inj (mod @Integer)),
        ("toString", inj E.toString),
        ("codepoint", inj (fromEnum . T.head)),
        ("char", inj (T.singleton . toEnum)),
        ("substr", inj substr),
        ("startsWith", inj (flip T.isPrefixOf)),
        ("endsWith", inj (flip T.isSuffixOf)),
        ("stripChars", inj stripChars),
        ("lstripChars", inj lstripChars),
        ("rstripChars", inj rstripChars),
        ("split", inj T.splitOn),
        ("strReplace", inj strReplace),
        ("asciiLower", inj T.toLower),
        ("asciiUpper", inj T.toUpper),
        ("stringChars", inj (T.chunksOf 1)),
        ("format", inj (printf @String)),
        ("parseInt", inj (read . T.unpack :: Text -> Int)),
        ("parseOctal", inj parseOctal),
        ("parseHex", inj parseHex),
        ("encodeUTF8", inj (B.unpack . T.encodeUtf8 :: Text -> [Word8])),
        ("decodeUTF8", inj (T.decodeUtf8 . B.pack :: [Word8] -> Text)),
        ("makeArray", inj makeArray),
        ("member", inj member),
        ("count", inj count),
        ("find", inj find'),
        ("map", inj (mapM @Vector @Eval @Value @Value)),
        ("mapWithIndex", inj mapWithIndex),
        ("filterMap", inj (filterMapM @Value @Value)),
        ("flatMap", inj (concatForM @Value @Value)), -- first function, then array
        ("filter", inj (filterM @Eval @Value)),
        ("foldl", inj (foldlM' @Vector @Value @Value)),
        ("foldr", inj (foldrM @Vector @Eval @Value @Value)),
        ("range", inj (enumFromTo @Int)),
        ("lines", inj T.unlines), -- yes, really
        ("repeat", inj repeat'),
        ("join", inj T.intercalate),
        ("reverse", inj (reverse @Value))
      ]

isType :: Text -> Value -> Bool
isType ty = (==) ty . valueType

length' :: Value -> Eval Int
length' = \case
  VStr s -> pure $ T.length s
  VArr a -> pure $ length a
  VObj o -> pure $ length (H.keys o)
  v ->
    throwError
      ( StdError $
          "length operates on strings, objects, and arrays, got "
            <> E.valueType v
      )

substr :: Text -> Int -> Int -> Text
substr str from len = T.take len $ T.drop from str

strReplace :: Text -> Text -> Text -> Text
strReplace str from to = T.replace from to str

containsChar :: Text -> Char -> Bool
containsChar s c = T.any (c ==) s

lstripChars :: Text -> Text -> Text
lstripChars s cs = T.dropWhile (containsChar cs) s

rstripChars :: Text -> Text -> Text
rstripChars s cs = T.dropWhileEnd (containsChar cs) s

stripChars :: Text -> Text -> Text
stripChars s cs = T.dropAround (containsChar cs) s

parseOctal :: Text -> Eval Integer
parseOctal num = case readOct (T.unpack num) of
  [(n, "")] -> pure n
  _ ->
    throwError
      ( StdError $
          num
            <> " is not a base 8 integer"
      )

parseHex :: Text -> Eval Integer
parseHex num = case readHex (T.unpack num) of
  [(n, "")] -> pure n
  _ ->
    throwError
      ( StdError $
          num
            <> " is not a base 16 integer"
      )

count :: [Value] -> Value -> Eval Int
count xs y = do
  xs' <- mapM manifest xs
  y' <- manifest y
  return $ length $ intersect xs' [y']

member :: [Value] -> Value -> Eval Bool
member xs y = (/= 0) <$> count xs y

find' :: Value -> [Value] -> Eval [Int]
find' y xs = liftA2 elemIndices (manifest y) (traverse manifest xs)

mapWithIndex :: (Value -> Int -> Eval Value) -> [Value] -> Eval [Value]
mapWithIndex f xs = zipWithM f xs [0 ..]

filterMapM :: (a -> Eval Bool) -> (a -> Eval b) -> [a] -> Eval [b]
filterMapM f g as = traverse g =<< filterM f as

makeArray :: Int -> (Int -> Eval Value) -> Eval [Value]
makeArray n f = traverse f [0 .. n - 1]

repeat' :: [Value] -> Int -> [Value]
repeat' xs times = join $ replicate times xs

concatForM :: (a -> Eval [b]) -> [a] -> Eval [b]
concatForM f xs = liftM concat (mapM f xs)

foldlM' :: Foldable t => (b -> a -> Eval b) -> t a -> b -> Eval b
foldlM' = flip . foldlM
