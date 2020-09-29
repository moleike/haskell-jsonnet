-- |
module Language.Jsonnet.JSON where

import Data.HashMap.Lazy (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data JSON
  = JNull
  | JBool !Bool
  | JNum !Double
  | JStr !Text
  | JArr !(Vector JSON)
  | JObj !(HashMap Text JSON)
  deriving (Eq, Show)
