-- |
module Language.Jsonnet.JSON where

import Data.HashMap.Lazy (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Scientific (Scientific)

data JSON
  = JNull
  | JBool !Bool
  | JNum !Scientific
  | JStr !Text
  | JArr !(Vector JSON)
  | JObj !(HashMap Text JSON)
  deriving (Eq, Show)
