{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Jsonnet.Pretty where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON (encodeToLazyText)
import qualified Data.HashMap.Lazy as H
import Data.List (sortOn)
import Data.Scientific (Scientific (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import qualified Data.Vector as V
import GHC.IO.Exception (IOException (..))
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Pos
import Text.PrettyPrint.ANSI.Leijen hiding (encloseSep)
import Unbound.Generics.LocallyNameless (Name, name2String)

instance Pretty (Name a) where
  pretty v = pretty (name2String v)

ppNumber s
  | e < 0 || e > 1024 =
    text
      $ LT.unpack
      $ toLazyText
      $ scientificBuilder s
  | otherwise = integer (coefficient s * 10 ^ e)
  where
    e = base10Exponent s

ppJson :: Int -> JSON.Value -> Doc
ppJson i =
  \case
    JSON.Null -> text "null"
    JSON.Number n -> ppNumber n
    JSON.Bool True -> text "true"
    JSON.Bool False -> text "false"
    JSON.String s -> ppString s
    JSON.Array a -> ppArray a
    JSON.Object o -> ppObject o
  where
    encloseSep l r s ds = case ds of
      [] -> l <> r
      _ -> l <$$> indent i (vcat $ punctuate s ds) <$$> r
    ppObject o = encloseSep lbrace rbrace comma xs
      where
        prop (k, v) = ppString k <> colon <+> ppJson i v
        xs = map prop (sortOn fst $ H.toList o)
    ppArray a = encloseSep lbracket rbracket comma xs
      where
        xs = map (ppJson i) (V.toList a)
    ppString = text . LT.unpack . JSON.encodeToLazyText

instance Pretty JSON.Value where
  pretty = ppJson 4

instance Pretty SrcSpan where
  pretty SrcSpan {spanBegin, spanEnd} =
    text (sourceName spanBegin)
      <> colon
      <> lc spanBegin
      <> comma
      <> lc spanEnd
    where
      lc pos =
        (int . unPos . sourceLine) pos
          <> colon
          <> (int . unPos . sourceColumn) pos

instance Pretty ParserError where
  pretty (ParseError e) = pretty (errorBundlePretty e)
  pretty (ImportError (IOError _ _ _ desc _ f) sp) =
    text "Parse error:"
      <+> pretty f
      <+> parens (text desc)

instance Pretty CheckError where
  pretty =
    \case
      DuplicateParam e sp ->
        text "duplicate parameter"
          <+> squotes (text e)
          <$$> indent 4 (pretty sp)
      DuplicateBinding e sp ->
        text "duplicate local var"
          <+> squotes (text e)
          <$$> indent 4 (pretty sp)
      PosAfterNamedParam sp ->
        text "positional after named argument"
          <$$> indent 4 (pretty sp)

instance Pretty EvalError where
  pretty =
    \case
      TypeMismatch expected actual sp ->
        text "type mismatch:"
          <+> text "expected"
          <+> text (T.unpack expected)
          <+> text "but got"
          <+> text (T.unpack actual)
          <$$> indent 4 (pretty sp)
      InvalidKey k sp ->
        text "invalid key:"
          <+> text (T.unpack k)
          <$$> indent 4 (pretty sp)
      InvalidIndex k sp ->
        text "invalid index:"
          <+> text (T.unpack k)
          <$$> indent 4 (pretty sp)
      NoSuchKey k sp ->
        text "no such key:"
          <+> text (T.unpack k)
          <$$> indent 4 (pretty sp)
      IndexOutOfBounds i sp ->
        text "index out of bounds:"
          <+> ppNumber i
          <$$> indent 4 (pretty sp)
      DivByZero sp ->
        text "divide by zero exception"
          <$$> indent 4 (pretty sp)
      VarNotFound v sp ->
        text "variable"
          <+> squotes (text $ show v)
          <+> text "is not defined"
          <$$> indent 4 (pretty sp)
      AssertionFailed e sp ->
        text "assertion failed:"
          <+> e
          <$$> indent 4 (pretty sp)
      StdError e sp -> e <$$> indent 4 (pretty sp)
      RuntimeError e sp ->
        text (T.unpack e)
          <$$> indent 4 (pretty sp)
      ParamNotBound s sp ->
        text "parameter not bound:"
          <+> text (show s)
          <$$> indent 4 (pretty sp)
      BadParam s sp ->
        text "function has no parameter"
          <+> text (T.unpack s)
          <$$> indent 4 (pretty sp)
      ManifestError e sp ->
        text "manifest error:"
          <+> text (T.unpack e)
          <$$> indent 4 (pretty sp)
      TooManyArgs n sp ->
        text "too many args, function has"
          <+> int n
          <+> "parameter(s)"
          <$$> indent 4 (pretty sp)

instance Pretty Error where
  pretty =
    \case
      EvalError e -> text "Runtime error:" <+> pretty e
      ParserError e -> pretty e
      CheckError e -> text "Static error:" <+> pretty e
