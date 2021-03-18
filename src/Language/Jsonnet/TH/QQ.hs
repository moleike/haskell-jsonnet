module Language.Jsonnet.TH.QQ where

import qualified Data.Text as T
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Jsonnet.TH

jsonnet :: QuasiQuoter
jsonnet =
  QuasiQuoter
    { quoteExp = \str -> parse "" (T.pack str),
      quotePat = error "Usage as a pattern is not supported",
      quoteType = error "Usage as a type is not supported",
      quoteDec = error "Usage as a declaration is not supported"
    }
