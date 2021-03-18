{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Jsonnet.Std.Lib
  ( std,
    objectHasEx,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Language.Jsonnet.Common
import Language.Jsonnet.Core (Fun (Fun), KeyValue (..))
import Language.Jsonnet.Error
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Value
import System.FilePath.Posix (takeFileName)
import Text.Megaparsec.Pos (SourcePos (..))
import Text.PrettyPrint.ANSI.Leijen (text)
import Unbound.Generics.LocallyNameless
import Prelude hiding (length)
import qualified Prelude as P (length)

-- | The native subset of Jsonnet standard library
std :: Value
std = VObj $ H.fromList $ map f xs
  where
    f = \(k, v) -> (k, TV <$> Hideable v Hidden)
    xs =
      ("thisFile", inj <$> thisFile) :
      map
        (\(k, v) -> (k, pure v))
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
          ("objectFieldsEx", inj objectFieldsEx),
          ("parseJson", inj (JSON.decodeStrict @Value))
        ]

primitiveEquals :: Value -> Value -> Eval Bool
primitiveEquals VNull VNull = pure True
primitiveEquals (VBool a) (VBool b) = pure (a == b)
primitiveEquals (VStr a) (VStr b) = pure (a == b)
primitiveEquals (VNum a) (VNum b) = pure (a == b)
primitiveEquals _ _ =
  throwE
    ( StdError $
        text $
          T.unpack $
            "primitiveEquals operates on primitive types"
    )

objectFieldsEx :: Object -> Bool -> [Text]
objectFieldsEx o True = sort (H.keys o) -- all fields
objectFieldsEx o False = sort $ H.keys $ H.filter (not . hidden) o -- only visible (incl. forced)

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
    throwE
      ( StdError $
          text $
            T.unpack $
              "length operates on strings, objects, functions and arrays, got "
                <> valueType v
      )

makeArray :: Int -> (Int -> Eval Value) -> Eval [Value]
makeArray n f = traverse f [0 .. n - 1]

-- hacky way of returning the current file
thisFile :: Eval (Maybe FilePath)
thisFile = f <$> gets currentPos
  where
    f = fmap (takeFileName . sourceName . spanBegin)

instance FromJSON Value where
  parseJSON = \case
    JSON.Null -> pure VNull
    JSON.Bool b -> pure $ VBool b
    JSON.Number n -> pure $ VNum n
    JSON.String s -> pure $ VStr s
    JSON.Array a -> VArr <$> traverse (fmap mkThunk' . parseJSON) a
    JSON.Object o -> VObj . f <$> traverse parseJSON o
    where
      f :: HashMap Text Value -> Object
      f o =
        H.fromList
          [ (mkField k (mkThunk' v))
            | (k, v) <- H.toList o
          ]
      mkField k v = (k, Hideable v Visible)
