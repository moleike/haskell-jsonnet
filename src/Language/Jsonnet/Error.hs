{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module                  : Language.Jsonnet.Error
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Error where

import Control.Exception
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Language.Jsonnet.Common
import Language.Jsonnet.Core
import Language.Jsonnet.Parser.SrcSpan
import Prettyprinter (Doc)
import Text.Megaparsec (ParseErrorBundle)

data Error
  = ParserError ParserError
  | CheckError CheckError (Maybe SrcSpan)
  | EvalError EvalError (Backtrace Core)
  deriving (Eq, Show)

instance Exception Error

data EvalError
  = TypeMismatch {expected :: Text, actual :: Text}
  | InvalidKey Text
  | DuplicateKey Text
  | NoSuchKey Text
  | InvalidIndex Text
  | IndexOutOfBounds Scientific
  | DivByZero
  | VarNotFound Text
  | AssertionFailed Text
  | TooManyArgs Int
  | ParamNotBound Text
  | BadParam Text
  | StdError Text
  | RuntimeError Text
  | ManifestError Text
  deriving (Eq, Show, Typeable)

instance Exception EvalError

data ParserError
  = ParseError (ParseErrorBundle Text Void)
  | ImportError IOError (Maybe SrcSpan)
  deriving (Eq, Show)

data CheckError
  = DuplicateParam String
  | PosAfterNamedParam
  | DuplicateBinding String
  deriving (Eq, Show)
