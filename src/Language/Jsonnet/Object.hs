{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Jsonnet.Object (
    Object,
    Field (..),
    mkBaseObject,
    visibleKeys,
    allKeys,
    regularKeys,
    assertionKeys,
    lookupField,
) where

import Data.Fix (Fix (..), foldFix)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Data.List qualified as L
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Jsonnet.Common (Visibility (..))
import Language.Jsonnet.Syntax.Annotated (para)
import {-# SOURCE #-} Language.Jsonnet.Value (Value)

data ObjectF r
    = Base !(HashMap Text Field)
    | Mixin r r
    deriving (Functor)

type Object = Fix ObjectF

instance Semigroup Object where
    Fix (Base m) <> r | H.null m = r
    l <> Fix (Base m) | H.null m = l
    l <> r = Fix (Mixin l r)

instance Monoid Object where
    mempty = Fix (Base H.empty)

data Field = Field
    { fieldKey :: Value
    , fieldVal :: Value
    , fieldVis :: Visibility
    , assertField :: Bool
    }
    deriving (Generic)

instance Semigroup Field where
    l <> r = r{fieldVis = fieldVis l <> fieldVis r}

mkBaseObject :: HashMap Text Field -> Object
mkBaseObject = Fix . Base

filterKeys :: (Field -> Bool) -> Object -> [Text]
filterKeys p = L.sort . H.keys . H.filter p . flattenFields

allKeys = filterKeys (const True)
visibleKeys = filterKeys (not . ((||) <$> (== Hidden) . fieldVis <*> assertField))
regularKeys = filterKeys (not . assertField)
assertionKeys = filterKeys assertField

flattenFields :: Object -> HashMap Text Field
flattenFields = foldFix $ \case
    Base m -> m
    Mixin l r -> H.unionWith (<>) l r

lookupField :: Text -> Object -> Maybe (Field, Maybe Object)
lookupField k = para $ \case
    Base m -> (,Nothing) <$> H.lookup k m
    Mixin (lTree, lRes) (_, rRes) ->
        case rRes of
            Just (f, rSuper) ->
                Just (f, Just (maybe lTree (lTree <>) rSuper))
            Nothing -> lRes
