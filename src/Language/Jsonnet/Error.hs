{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Jsonnet.Error where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Language.Jsonnet.Parser.SrcSpan
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Unbound.Generics.LocallyNameless (AnyName)
import Text.Megaparsec (ParseErrorBundle)
import Data.Void (Void)

data EvalError
  = TypeMismatch {expected :: Text, actual :: Text}
  | InvalidKey Text
  | DuplicateKey Text
  | NoSuchKey Text
  | InvalidIndex Text
  | IndexOutOfBounds Scientific
  | DivByZero
  | VarNotFound AnyName
  | AssertionFailed Doc
  | TooManyArgs Int
  | ParamNotBound AnyName
  | BadParam Text
  | StdError Doc
  | RuntimeError Text
  | ManifestError Text
  deriving (Show)

data ParserError
  = ParseError (ParseErrorBundle Text Void)
  | ImportError IOError (Maybe SrcSpan)
  deriving (Eq, Show)

data CheckError
  = NotFound (Maybe SrcSpan) String
  | PosAfterNamedParam (Maybe SrcSpan) String
  | DuplicateParam (Maybe SrcSpan) String
  | DuplicateBinding (Maybe SrcSpan) String
  | SelfOutOfBounds (Maybe SrcSpan)
  deriving (Show)

data Error
  = ParserError ParserError
  | CheckError CheckError
  | EvalError EvalError (Maybe SrcSpan)
  deriving (Show)
