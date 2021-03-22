{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Language.Jsonnet.Value where

import Control.Applicative
import Control.Arrow
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Bits
import Data.ByteString (ByteString)
import Data.Data
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Hashable (Hashable)
import Data.IORef
import Data.Int
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Error
import {-# SOURCE #-} Language.Jsonnet.Eval (eval, evalClos)
import Language.Jsonnet.Eval.Monad
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Pretty ()
import Text.PrettyPrint.ANSI.Leijen (Doc, pretty)
import Unbound.Generics.LocallyNameless

-- jsonnet value
data Value
  = VNull
  | VBool !Bool
  | VNum !Scientific
  | VStr !Text
  | VArr !Array
  | VObj !(HashMap Text (Hideable Thunk))
  | VClos !Fun !Env
  | VFun !(Thunk -> Eval Value)
  deriving (Generic)

type Array = Vector Thunk

type Object = HashMap Text (Hideable Thunk)

valueType :: Value -> Text
valueType =
  \case
    VNull -> "null"
    VBool _ -> "boolean"
    VNum _ -> "number"
    VStr _ -> "string"
    VArr _ -> "array"
    VObj _ -> "object"
    VClos _ _ -> "function"
    VFun _ -> "function"

data Thunk = TC !Ctx !Core | TV !(Eval Value)
  deriving (Generic)

force :: Thunk -> Eval Value
force = \case
  TC rho expr -> withCtx rho (eval expr)
  TV comp -> comp

mkThunk' :: Value -> Thunk
mkThunk' = TV . pure

mkThunk :: MonadIO m => Eval Value -> m Thunk
mkThunk ev = do
  ref <- liftIO $ newIORef Nothing
  pure $
    TV $
      liftIO (readIORef ref) >>= \case
        Nothing -> do
          v <- ev
          liftIO $ writeIORef ref $ Just v
          pure v
        Just v -> pure v

proj' :: HasValue a => Thunk -> Eval a
proj' = force >=> proj

class HasValue a where
  proj :: Value -> Eval a
  inj :: a -> Value

instance {-# OVERLAPS #-} HasValue Value where
  proj = pure
  inj = id

instance HasValue Bool where
  proj (VBool n) = pure n
  proj v = throwTypeMismatch "bool" v
  inj = VBool

instance HasValue Text where
  proj (VStr s) = pure s
  proj v = throwTypeMismatch "string" v
  inj = VStr

instance {-# OVERLAPPING #-} HasValue [Char] where
  proj (VStr s) = pure $ T.unpack s
  proj v = throwTypeMismatch "string" v
  inj = VStr . T.pack

instance HasValue ByteString where
  proj (VStr s) = pure (encodeUtf8 s)
  proj v = throwTypeMismatch "string" v
  inj = VStr . decodeUtf8

instance HasValue Scientific where
  proj (VNum n) = pure n
  proj v = throwTypeMismatch "number" v
  inj = VNum

instance HasValue Double where
  proj (VNum n) = pure (toRealFloat n)
  proj v = throwTypeMismatch "number" v
  inj = VNum . fromFloatDigits

instance {-# OVERLAPS #-} Integral a => HasValue a where
  proj (VNum n) = pure (round n)
  proj v = throwTypeMismatch "number" v
  inj = VNum . fromIntegral

instance HasValue a => HasValue (Maybe a) where
  proj VNull = pure Nothing
  proj a = Just <$> proj a
  inj Nothing = VNull
  inj (Just a) = inj a

instance HasValue a => HasValue (Vector a) where
  proj (VArr as) = traverse proj' as
  proj v = throwTypeMismatch "array" v
  inj as = VArr $ mkThunk' . inj <$> as

instance {-# OVERLAPS #-} HasValue (Vector Thunk) where
  proj (VArr as) = pure as
  proj v = throwTypeMismatch "array" v
  inj = VArr

instance {-# OVERLAPPABLE #-} HasValue a => HasValue [a] where
  proj = fmap V.toList . proj
  inj = inj . V.fromList

instance {-# OVERLAPS #-} HasValue Object where
  proj (VObj o) = pure o
  proj v = throwTypeMismatch "object" v
  inj = VObj

--instance HasValue a => HasValue (Object a) where
--  proj (VObj o) = traverse proj' o
--  proj v = throwTypeMismatch "object" v
--  inj o = VObj $ mkThunk' . inj <$> o

instance {-# OVERLAPS #-} (HasValue a, HasValue b) => HasValue (a -> b) where
  proj v = throwTypeMismatch "impossible" v
  inj f = VFun $ \x -> force x >>= fmap (inj . f) . proj

instance {-# OVERLAPS #-} (HasValue a, HasValue b, HasValue c) => HasValue (a -> b -> c) where
  proj v = throwTypeMismatch "impossible" v
  inj f = inj $ \x -> inj (f x)

instance {-# OVERLAPS #-} (HasValue a, HasValue b) => HasValue (a -> Eval b) where
  proj (VFun f) = pure $ \x -> do
    r <- f (mkThunk' $ inj x)
    proj r
  proj (VClos f env) = pure $ \x -> do
    r <- evalClos (ctx env) f $ [Pos $ mkThunk' $ inj x]
    proj r
  proj v = throwTypeMismatch "function" v
  inj f = VFun $ \v -> proj' v >>= fmap inj . f

instance {-# OVERLAPS #-} (HasValue a, HasValue b, HasValue c) => HasValue (a -> b -> Eval c) where
  proj (VFun f) = pure $ \x y -> do
    VFun g <- f (mkThunk' $ inj x)
    r <- g (mkThunk' $ inj y)
    proj r
  proj (VClos f env) = pure $ \x y -> do
    r <- evalClos (ctx env) f $ Pos . mkThunk' <$> [inj x, inj y]
    proj r
  proj v = throwTypeMismatch "function" v
  inj f = inj $ \x -> inj (f x)

throwTypeMismatch :: Text -> Value -> Eval a
throwTypeMismatch expected =
  throwE
    . TypeMismatch expected
    . valueType

inj' ::
  (HasValue a, HasValue b) =>
  (a -> b) ->
  (Value -> Eval Value)
inj' f v = inj . f <$> proj v

inj'' ::
  (HasValue a, HasValue b, HasValue c) =>
  (a -> b -> c) ->
  Value ->
  Value ->
  Eval Value
inj'' f v1 v2 = inj <$> liftA2 f (proj v1) (proj v2)
