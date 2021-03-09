{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
module Language.Jsonnet.Object where

import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import Language.Jsonnet.Common
import Unbound.Generics.LocallyNameless
