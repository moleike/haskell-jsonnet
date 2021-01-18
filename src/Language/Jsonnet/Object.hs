{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
-- |

module Language.Jsonnet.Object where

import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import Unbound.Generics.LocallyNameless

data Visibility
  = Visible
  | Hidden
  | Forced
  deriving
    ( Eq,
      Read,
      Show,
      Generic
    )

instance Alpha Visibility

newtype Key a = Key { key :: a }
  deriving
    ( Eq,
      Read,
      Show,
      Typeable,
      Generic,
      Generic1,
      Functor,
      Foldable,
      Traversable
    )

instance Alpha a => Alpha (Key a)

data Value a = Value { value :: a, visibility :: Visibility }
  deriving
    ( Eq,
      Read,
      Show,
      Typeable,
      Generic,
      Generic1,
      Functor,
      Foldable,
      Traversable
    )

instance Alpha a => Alpha (Value a)

visible (Value _ Visible) = True
visible _ = False

forced (Value _ Forced) = True
forced _ = False

hidden (Value _ Hidden) = True
hidden _ = False


data Field a = Field !(Key a) !(Value a)
  deriving
    ( Eq,
      Read,
      Show,
      Typeable,
      Generic,
      Generic1,
      Functor,
      Foldable,
      Traversable
    )

instance Alpha a => Alpha (Field a)
