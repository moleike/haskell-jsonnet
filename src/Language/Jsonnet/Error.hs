{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Jsonnet.Error where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Void (Void)
import Language.Jsonnet.Parser.SrcSpan
import Text.Megaparsec (ParseErrorBundle)
import Text.PrettyPrint.ANSI.Leijen (Doc)

data Error
  = ParserError ParserError
  | CheckError CheckError (Maybe SrcSpan)
  | EvalError EvalError (Maybe SrcSpan)
  deriving (Show)

data EvalError
  = TypeMismatch {expected :: Text, actual :: Text}
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
  deriving (Show)

data ParserError
  = ParseError (ParseErrorBundle Text Void)
  | ImportError IOError (Maybe SrcSpan)
  deriving (Eq, Show)

data CheckError
  = DuplicateParam String
  | PosAfterNamedParam
  | DuplicateBinding String
  deriving (Show)
