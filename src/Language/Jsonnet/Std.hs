{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Jsonnet.Std
  ( std,
    objectHasEx,
  )
where

import Control.Monad.Except
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as H
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import Data.Word
import Language.Jsonnet.Core (Fun (Fun))
import Language.Jsonnet.Error
import Language.Jsonnet.Eval.Monad
import qualified Language.Jsonnet.Object as O
import Language.Jsonnet.Value
import Text.PrettyPrint.ANSI.Leijen (text)
import Unbound.Generics.LocallyNameless
import qualified Prelude as P (length)
import Prelude hiding (length)

-- The native subset of Jsonnet standard library
std :: Value
std = VObj $ (fmap (TV . pure)) <$> H.fromList xs
  where
    xs :: [(O.Key Text, O.Value Value)]
    xs =
      map
        (\(k, v) -> (O.Key k, O.Value v O.Hidden))
        [ ("type", inj valueType),
          ("primitiveEquals", inj primitiveEquals),
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
          ("makeArray", inj makeArray),
          ("filter", inj (filterM @Eval @Value)),
          ("objectHasEx", inj objectHasEx),
          ("objectFieldsEx", inj objectFieldsEx)
        ]

primitiveEquals :: Value -> Value -> Eval Bool
primitiveEquals VNull VNull = pure True
primitiveEquals (VBool a) (VBool b) = pure (a == b)
primitiveEquals (VStr a) (VStr b) = pure (a == b)
primitiveEquals (VNum a) (VNum b) = pure (a == b)
primitiveEquals _ _ =
  throwError
    ( StdError
        $ text
        $ T.unpack
        $ "primitiveEquals operates on primitive types"
    )

objectFieldsEx :: Object -> Bool -> [Text]
objectFieldsEx o True = sort (O.key <$> H.keys o) -- all fields
objectFieldsEx o False = sort $ fmap O.key $ H.keys $ H.filter (not . O.hidden) o -- only visible (incl. forced)

objectHasEx :: Object -> Text -> Bool -> Bool
objectHasEx o f all = f `elem` objectFieldsEx o all

length :: Value -> Eval Int
length = \case
  VStr s -> pure $ T.length s
  VArr a -> pure $ P.length a
  VObj o -> pure $ P.length (H.keys o)
  VClos (Fun f) _ -> do
    (ps, _) <- unbind f
    pure $ P.length (unrec ps)
  v ->
    throwError
      ( StdError
          $ text
          $ T.unpack
          $ "length operates on strings, objects, functions and arrays, got "
            <> valueType v
      )

makeArray :: Int -> (Int -> Eval Value) -> Eval [Value]
makeArray n f = traverse f [0 .. n - 1]
