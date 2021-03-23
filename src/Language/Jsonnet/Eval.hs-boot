module Language.Jsonnet.Eval where

import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Eval.Monad
import {-# SOURCE #-} Language.Jsonnet.Value (Thunk, Value)

eval :: Core -> Eval Value
evalClos :: Ctx -> Fun -> [Arg Thunk] -> Eval Value
