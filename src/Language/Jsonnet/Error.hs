{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Jsonnet.Error where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Language.Jsonnet.Parser (ParseError)
import Language.Jsonnet.Parser.SrcSpan
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Unbound.Generics.LocallyNameless (AnyName)

data EvalError
  = TypeMismatch {expected :: Text, actual :: Text}
  | InvalidKey Text
  | DuplicateKey Text
  | NoSuchKey Text
  | InvalidIndex Text
  | IndexOutOfBounds Scientific
  | DivByZero
  | VarNotFound AnyName
  | ManifestError ManifestError
  | AssertionFailed Doc
  | TooManyArgs Int
  | ParamNotBound AnyName
  | BadParam Text
  | StdError Doc
  | RuntimeError Text
  deriving (Show)

newtype ManifestError = NotAJsonValue Text
  deriving (Show)

data Error
  = ParserError ParseError
  | EvalError EvalError (Maybe SrcSpan)
  deriving (Show)
