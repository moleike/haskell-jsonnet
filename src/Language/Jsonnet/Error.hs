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
import Text.Megaparsec (ParseErrorBundle)
import Text.PrettyPrint.ANSI.Leijen (Doc)

data Error
  = ParserError ParserError
  | CheckError CheckError (Maybe SrcSpan)
  | EvalError EvalError (Backtrace Core)
  deriving (Show)

data EvalError
  = TypeMismatch
      { expected :: Text,
        actual :: Text
      }
  | InvalidKey Doc
  | DuplicateKey Doc
  | NoSuchKey Doc
  | InvalidIndex Doc
  | IndexOutOfBounds Scientific
  | DivByZero
  | VarNotFound Doc
  | AssertionFailed Doc
  | TooManyArgs Int
  | ParamNotBound Doc
  | BadParam Doc
  | StdError Doc
  | RuntimeError Doc
  | ManifestError Doc
  deriving (Show, Typeable)

instance Exception EvalError

data ParserError
  = ParseError (ParseErrorBundle Text Void)
  | ImportError IOError (Maybe SrcSpan)
  deriving (Eq, Show)

data CheckError
  = DuplicateParam String
  | PosAfterNamedParam
  | DuplicateBinding String
  deriving (Show)
