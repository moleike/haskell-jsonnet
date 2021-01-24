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
import Unbound.Generics.LocallyNameless (AnyName)

data EvalError
  = TypeMismatch Text Text (Maybe SrcSpan)
  | InvalidKey Text (Maybe SrcSpan)
  | DuplicateKey Text (Maybe SrcSpan)
  | NoSuchKey Text (Maybe SrcSpan)
  | InvalidIndex Text (Maybe SrcSpan)
  | IndexOutOfBounds Scientific (Maybe SrcSpan)
  | DivByZero (Maybe SrcSpan)
  | VarNotFound AnyName (Maybe SrcSpan)
  | AssertionFailed Doc (Maybe SrcSpan)
  | TooManyArgs Int (Maybe SrcSpan)
  | ParamNotBound AnyName (Maybe SrcSpan)
  | BadParam Text (Maybe SrcSpan)
  | StdError Doc (Maybe SrcSpan)
  | RuntimeError Text (Maybe SrcSpan)
  | ManifestError Text (Maybe SrcSpan)
  deriving (Show)

data ParserError
  = ParseError (ParseErrorBundle Text Void)
  | ImportError IOError (Maybe SrcSpan)
  deriving (Eq, Show)

data CheckError
  = NotFound String (Maybe SrcSpan)
  | PosAfterNamedParam (Maybe SrcSpan)
  | DuplicateParam String (Maybe SrcSpan)
  | DuplicateBinding String (Maybe SrcSpan)
  | SelfOutOfBounds (Maybe SrcSpan)
  deriving (Show)

data Error
  = ParserError ParserError
  | CheckError CheckError
  | EvalError EvalError
  deriving (Show)
