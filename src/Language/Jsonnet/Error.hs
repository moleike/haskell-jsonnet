{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Jsonnet.Error where

import Data.Text (Text)
import Language.Jsonnet.Parser (ParseError)
import Language.Jsonnet.Parser.SrcSpan
import Text.PrettyPrint.ANSI.Leijen (Doc)

data EvalError
  = TypeMismatch {expected :: Text, actual :: Text}
  | InvalidKey Text
  | NoSuchKey Text
  | IndexOutOfBounds Int
  | DivByZero
  | VarNotFound Text
  | ManifestError ManifestError
  | AssertionFailed Text
  | StdError Text
  deriving (Show)

newtype ManifestError = NotAJsonValue Text
  deriving (Show)

data Error
  = ParserError ParseError
  | EvalError EvalError (Maybe SrcSpan)
  deriving (Show)
