{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module                  : Language.Jsonnet.Std.Lib
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Std.Lib
  ( std,
    objectHasEx,
  )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as JSON
import Data.ByteString qualified as B
import Data.Functor
import Data.HashMap.Lazy qualified as H
import Data.List qualified as L (intercalate)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import Language.Jsonnet.Common
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Value
import System.IO
import Unbound.Generics.LocallyNameless
import Prelude hiding (length)
import Prelude qualified as P (length)

-- | Jsonnet standard library built-in methods
std :: ExtVars -> Value
std extVars = VObj $ H.fromList $ map f xs
  where
    f = \(k, v) -> (k, VField (VStr k) v v Hidden False)
    xs =
      [ ("type", inj showTy),
        ("primitiveEquals", inj primitiveEquals),
        ("equals", inj equals),
        ("length", inj length),
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
        ("atan2", inj (atan2 @Double)),
        ("hypot", inj hypot),
        ("modulo", inj (mod @Integer)),
        ("codepoint", inj (fromEnum . T.head)),
        ("char", inj (T.singleton . toEnum)),
        ("encodeUTF8", inj (B.unpack . T.encodeUtf8 :: Text -> [Word8])),
        ("decodeUTF8", inj (T.decodeUtf8 . B.pack :: [Word8] -> Text)),
        ("makeArray", inj (makeArray @Eval)),
        ("filter", inj (V.filterM @Eval @Value)),
        ("join", inj intercalate),
        ("objectHasEx", inj objectHasEx),
        ("objectFieldsEx", inj objectFieldsEx),
        ("parseJson", inj (JSON.decodeStrict @Value)),
        ("extVar", inj (lookupExtVar extVars)),
        ("thisFile", inj (getFilename @Value)),
        ("trace", inj trace)
      ]

lookupExtVar :: ExtVars -> Text -> Eval Value
lookupExtVar (ExtVars extVars) s = liftMaybe (ExtVarNotFound s) (M.lookup s extVars)

intercalate :: Value -> [Value] -> Eval Value
intercalate sep arr = go sep (filter notNull arr)
  where
    notNull VNull = False
    notNull _ = True
    go sep'@(VArr _) = app (L.intercalate @Value) sep'
    go sep'@(VStr _) = app T.intercalate sep'
    go _ = const $ throwE (StdError "join's separator must be a string or an array")
    app f sep' arr' = inj <$> (f <$> proj sep' <*> traverse proj arr')

length :: Value -> Eval Int
length = \case
  VStr s -> pure $ T.length s
  VArr a -> pure $ P.length a
  VObj o -> pure $ P.length (objectFieldsEx o True)
  VClos f _ -> do
    (ps, _) <- unbind f
    pure $ P.length (unrec ps)
  _ ->
    throwE
      ( StdError "length operates on strings, objects, functions and arrays, got "
      )

makeArray :: (Monad m) => Int -> (Int -> m Value) -> m (Vector Value)
makeArray n f = traverse f (V.fromList [0 .. n - 1])

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

trace :: String -> Value -> Eval Value
trace str = ($>) (msg >>= liftIO . hPutStrLn stderr)
  where
    msg = do
      fn <- getFilename <&> mfilter (not . null) -- exec mode has no filename
      ln <- getLineNum <&> fmap show
      fl <- pure $ fromMaybe mempty $ liftA2 (++) ((++ ":") <$> fn) ((++ " ") <$> ln)
      pure $ "TRACE: " ++ fl ++ str
