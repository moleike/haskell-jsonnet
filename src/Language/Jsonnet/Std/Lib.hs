{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module                  : Language.Jsonnet.Std.Lib
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Std.Lib (
    std,
    objectHasEx,
    ExtVars (..),
    UserFuncs (..),
)
where

import Control.Lens (view)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as JSON
import Data.ByteString qualified as B
import Data.Functor
import Data.HashMap.Lazy qualified as H
import Data.List qualified as L (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Language.Jsonnet.Common
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Value
import System.IO
import Unbound.Generics.LocallyNameless
import Prelude hiding (length)
import Prelude qualified as P (length)

newtype ExtVars = ExtVars (Map Text Value)
    deriving newtype (Semigroup, Monoid)

newtype UserFuncs = UserFuncs {unUserFuncs :: Map Text Value}
    deriving newtype (Semigroup, Monoid)

-- | Jsonnet standard library built-in methods
std :: ExtVars -> UserFuncs -> Value
std (ExtVars extVars) (UserFuncs userFuncs) = VObj (H.fromList xs)
  where
    infixr 8 ~>
    k ~> v = (k, VField (VStr k) (inj v) (inj v) Hidden False)
    xs =
        [ "type" ~> showTy
        , "primitiveEquals" ~> primitiveEquals
        , "equals" ~> equals
        , "length" ~> length
        , "pow" ~> (^^) @Double @Int
        , "exp" ~> exp @Double
        , "log" ~> log @Double
        , "exponent" ~> exponent @Double
        , "mantissa" ~> significand @Double
        , "floor" ~> floor @Double @Integer
        , "ceil" ~> ceiling @Double @Integer
        , "sqrt" ~> sqrt @Double
        , "sin" ~> sin @Double
        , "cos" ~> cos @Double
        , "tan" ~> tan @Double
        , "asin" ~> asin @Double
        , "acos" ~> acos @Double
        , "atan" ~> atan @Double
        , "atan2" ~> atan2 @Double
        , "hypot" ~> \a b -> (sqrt @Double) (a * a + b * b)
        , "modulo" ~> mod @Integer
        , "codepoint" ~> fromEnum . T.head
        , "char" ~> T.singleton . toEnum
        , "encodeUTF8" ~> B.unpack . T.encodeUtf8
        , "decodeUTF8" ~> T.decodeUtf8 . B.pack
        , "makeArray" ~> V.generateM @Eval @Value
        , "filter" ~> V.filterM @Eval @Value
        , "join" ~> intercalate
        , "objectHasEx" ~> objectHasEx
        , "objectFieldsEx" ~> objectFieldsEx
        , "parseJson" ~> JSON.decodeStrict @JSON.Value
        , "extVar" ~> (liftMaybe . ExtVarNotFound <*> (`Map.lookup` extVars))
        , "thisFile" ~> getFileName @Value
        , "trace" ~> trace
        , "native" ~> (`Map.lookup` userFuncs)
        ]

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
            (StdError "length operates on strings, objects, functions and arrays, got ")

trace :: String -> Value -> Eval Value
trace str =
    ($>)
        ( do
            fn <- view currentFile
            ln <- getLineNum
            let loc = case ln of
                    Just l -> fn ++ ":" ++ show l
                    Nothing -> fn
            liftIO $ hPutStrLn stderr ("TRACE: " ++ loc ++ " " ++ str)
        )
