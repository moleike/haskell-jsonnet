{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as B
import Data.Foldable (foldrM)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import qualified Data.List as L (intercalate, sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Value
import System.FilePath.Posix (takeFileName)
import Text.Megaparsec.Pos (SourcePos (..))
import Unbound.Generics.LocallyNameless
import Prelude hiding (length)
import qualified Prelude as P (length)

-- | Jsonnet standard library built-in methods
std :: ExtVars -> Value
std extVars = VObj $ H.fromList $ map f xs
  where
    f = \(k, v) -> (k, VField (VStr k) v v Hidden)
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
        ("extVar", inj (lookupExtVar extVars))
      ]

lookupExtVar :: ExtVars -> Text -> Eval Value
lookupExtVar (ExtVars extVars) s = liftMaybe (ExtVarNotFound s) (M.lookup s extVars)

intercalate :: Value -> [Value] -> Eval Value
intercalate sep arr = go sep (filter null arr)
  where
    null VNull = False
    null _ = True
    go sep@(VArr _) = app (L.intercalate @Value) sep
    go sep@(VStr _) = app T.intercalate sep
    app f sep arr = inj <$> (f <$> proj sep <*> traverse proj arr)

length :: Value -> Eval Int
length = \case
  VStr s -> pure $ T.length s
  VArr a -> pure $ P.length a
  VObj o -> pure $ P.length (H.keys o)
  VClos f _ -> do
    (ps, _) <- unbind f
    pure $ P.length (unrec ps)
  v ->
    throwE
      ( StdError "length operates on strings, objects, functions and arrays, got "
      --   <> showTy v
      )

makeArray :: Monad m => Int -> (Int -> m Value) -> m (Vector Value)
makeArray n f = traverse f (V.fromList [0 .. n - 1])

instance FromJSON Value where
  parseJSON = \case
    JSON.Null -> pure VNull
    JSON.Bool b -> pure $ VBool b
    JSON.Number n -> pure $ VNum n
    JSON.String s -> pure $ VStr s
    JSON.Array a -> VArr <$> traverse parseJSON a
    JSON.Object o -> VObj . f <$> traverse parseJSON (KeyMap.toHashMapText o)
    where
      f :: HashMap Text Value -> Object
      f o =
        H.fromList
          [ mkField k v
            | (k, v) <- H.toList o
          ]
      mkField k v = (k, VField (VStr k) v v Visible)
