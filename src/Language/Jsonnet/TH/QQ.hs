-- |
-- Module                  : Language.Jsonnet.TH.QQ
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.TH.QQ where

import qualified Data.Text as T
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Jsonnet.TH

jsonnet :: QuasiQuoter
jsonnet =
  QuasiQuoter
    { quoteExp = parse "" . T.pack,
      quotePat = error "Usage as a pattern is not supported",
      quoteType = error "Usage as a type is not supported",
      quoteDec = error "Usage as a declaration is not supported"
    }
