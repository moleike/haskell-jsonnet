{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson qualified as JSON
import Data.Binary (decode)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.Fixed (mod')
import Data.Functor
import Data.HashMap.Lazy qualified as H
import Data.IORef
import Data.List qualified as L (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Language.Jsonnet.Common (BinOp (Add))
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Eval.Types
import Language.Jsonnet.Object
import Language.Jsonnet.Std.TH (mkStdlib)
import Language.Jsonnet.Value
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Unbound.Generics.LocallyNameless
import Prelude hiding (length)
import Prelude qualified as P (length)

newtype ExtVars = ExtVars (Map Text Value)
    deriving newtype (Semigroup, Monoid)

newtype UserFuncs = UserFuncs {unUserFuncs :: Map Text Value}
    deriving newtype (Semigroup, Monoid)

{-# NOINLINE nonnative #-}
nonnative :: Value
nonnative =
    unsafePerformIO $
        runEvalM "<stdlib>" dummyImporter Map.empty (whnf core)
  where
    core = decode $(mkStdlib)
    dummyImporter = Importer $ \_ _ ->
        fail "Fatal: std.jsonnet attempted to import a file!"

{- | the jsonnet stdlib is written in both jsonnet and Haskell, here we merge
  the native (a small subset) with the interpreted (the splice mkStdlib)
-}
std :: (MonadIO m) => ExtVars -> UserFuncs -> m Value
std (ExtVars extVars) (UserFuncs userFuncs) =
    mkIndirV $ VThunk $ do
        cache <- liftIO (newIORef H.empty)
        let native = VObj (mkBaseObject (H.fromList xs)) cache
        whnfBinOp Add nonnative native
  where
    infixr 8 ~>
    k ~> v = (k, mkHiddenField k v)
    xs =
        [ "type" ~> showTy
        , "primitiveEquals" ~> primitiveEquals
        , "equals" ~> equals
        , "length" ~> length
        , "pow" ~> (**) @Double
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
        , "modulo" ~> mod' @Double
        , "codepoint" ~> fromEnum . T.head
        , "char" ~> T.singleton . toEnum
        , "encodeUTF8" ~> B.unpack . T.encodeUtf8
        , "decodeUTF8" ~> T.decodeUtf8 . B.pack
        , "makeArray" ~> V.generateM @Eval @Value
        , "filter" ~> V.filterM @Eval @Value
        , "join" ~> intercalate
        , "objectHasEx" ~> objectHasEx
        , "objectFieldsEx" ~> objectFieldsEx
        , "objectRemoveKey" ~> flip (H.delete @Text @Value)
        , "parseJson" ~> JSON.throwDecodeStrict @JSON.Value @Eval
        , "parseYaml" ~> parseYaml
        , "extVar" ~> (liftMaybe . ExtVarNotFound <*> (`Map.lookup` extVars))
        , "thisFile" ~> getFileName
        , "trace" ~> trace
        , "native" ~> (`Map.lookup` userFuncs)
        , "md5" ~> T.decodeUtf8 . B16.encode . MD5.hash . T.encodeUtf8
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
    VObj o _ -> pure $ P.length (objectFieldsEx o True)
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

parseYaml :: B.ByteString -> Eval Value
parseYaml bs =
    Yaml.decodeAllThrow @Eval @JSON.Value bs <&> \case
        [] -> VNull
        [doc] -> inj doc
        docs -> inj docs
