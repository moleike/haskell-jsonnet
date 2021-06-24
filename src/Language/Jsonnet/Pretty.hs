{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module                  : Language.Jsonnet.Pretty
-- Copyright               : (c) 2020-2021 Alexandre Moreno
-- SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
-- Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
-- Stability               : experimental
-- Portability             : non-portable
module Language.Jsonnet.Pretty where

import Control.Applicative (Const (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON (encodeToLazyText)
import Data.Fix
import Data.Functor.Sum
import qualified Data.HashMap.Lazy as H
import Data.List (sortOn)
import Data.Scientific (Scientific (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import qualified Data.Vector as V
import Data.Void (Void)
import GHC.IO.Exception (IOException (..))
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Language.Jsonnet.Syntax.Annotated
import Prettyprinter
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Pos
import Unbound.Generics.LocallyNameless (Name, name2String)

(<$$>) :: Doc ann -> Doc ann -> Doc ann
(<$$>) = \x y -> vcat [x, y]

-- reserved keywords
pnull, ptrue, pfalse, pif, pthen, pelse, pimport :: Doc ann
pnull = pretty "null"
ptrue = pretty "true"
pfalse = pretty "false"
pif = pretty "if"
pthen = pretty "then"
pelse = pretty "else"
pimport = pretty "import"

instance Pretty (Name a) where
  pretty v = pretty (name2String v)

ppNumber :: Scientific -> Doc ann
ppNumber s
  | e < 0 || e > 1024 =
    pretty $
      LT.unpack $
        toLazyText $
          scientificBuilder s
  | otherwise = pretty (coefficient s * 10 ^ e)
  where
    e = base10Exponent s

ppJson :: Int -> JSON.Value -> Doc ann
ppJson i = \case
  JSON.Null -> pnull
  JSON.Number n -> ppNumber n
  JSON.Bool True -> ptrue
  JSON.Bool False -> pfalse
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
    ppString = pretty . LT.unpack . JSON.encodeToLazyText

instance Pretty JSON.Value where
  pretty = ppJson 4

instance Pretty SrcSpan where
  pretty SrcSpan {spanBegin, spanEnd} =
    pretty (sourceName spanBegin)
      <> colon
      <> lc spanBegin spanEnd
    where
      lc (SourcePos _ lb cb) (SourcePos _ le ce)
        | lb == le =
          pretty (unPos lb) <> colon
            <> pretty (unPos cb)
            <> dash
            <> pretty (unPos ce)
        | otherwise =
          pretty (unPos lb) <> colon <> pretty (unPos cb) <> dash
            <> pretty (unPos le)
            <> colon
            <> pretty (unPos ce)
      dash = pretty '-'

instance Pretty ParserError where
  pretty (ParseError e) = pretty (errorBundlePretty e)
  pretty (ImportError (IOError _ _ _ desc _ f) sp) =
    pretty "Parse error:"
      <+> pretty f
      <+> parens (pretty desc)
      <$$> indent 4 (pretty sp)

instance Pretty CheckError where
  pretty = \case
    DuplicateParam e ->
      pretty "duplicate parameter"
        <+> squotes (pretty e)
    DuplicateBinding e ->
      pretty "duplicate local var"
        <+> squotes (pretty e)
    PosAfterNamedParam ->
      pretty "positional after named argument"

instance Pretty EvalError where
  pretty = \case
    TypeMismatch {..} ->
      pretty "type mismatch:"
        <+> pretty "expected"
        <+> pretty (T.unpack expected)
        <+> pretty "but got"
        <+> pretty (T.unpack actual)
    InvalidKey k ->
      pretty "invalid key:"
        <+> pretty k
    InvalidIndex k ->
      pretty "invalid index:"
        <+> pretty k
    NoSuchKey k ->
      pretty "no such key:"
        <+> pretty k
    IndexOutOfBounds i ->
      pretty "index out of bounds:"
        <+> ppNumber i
    DivByZero ->
      pretty "divide by zero exception"
    VarNotFound v ->
      pretty "variable"
        <+> squotes (pretty $ show v)
        <+> pretty "is not defined"
    AssertionFailed e ->
      pretty "assertion failed:" <+> pretty e
    StdError e -> pretty e
    RuntimeError e -> pretty e
    ParamNotBound s ->
      pretty "parameter not bound:"
        <+> pretty (show s)
    BadParam s ->
      pretty "function has no parameter"
        <+> squotes (pretty s)
    ManifestError e ->
      pretty "manifest error:"
        <+> pretty e
    TooManyArgs n ->
      pretty "too many args, function has"
        <+> pretty n
        <+> pretty "parameter(s)"

instance Pretty (StackFrame a) where
  pretty StackFrame {..} =
    pretty span <+> (f $ name2String name)
    where
      f "top-level" = mempty
      f x = pretty "function" <+> (angles $ pretty x)

instance Pretty (Backtrace a) where
  pretty (Backtrace xs) = vcat $ pretty <$> xs

instance Pretty Error where
  pretty = \case
    EvalError e bt ->
      pretty "Runtime error:"
        <+> pretty e
        <$$> indent 2 (pretty bt)
    ParserError e -> pretty e
    CheckError e sp ->
      pretty "Static error:"
        <+> pretty e
        <$$> indent 2 (pretty sp)

instance Pretty Literal where
  pretty = \case
    Null -> pnull
    Bool True -> ptrue
    Bool False -> pfalse
    -- FIXME strings need extra source info (quotes, verbatim, etc.)
    String t -> pretty t
    Number n -> ppNumber n

-- TODO unfinished printer for AST
instance Pretty Expr' where
  pretty = foldFix go
    where
      go (AnnF (InR (Const (Import fp))) _) = pimport <+> pretty fp
      go (AnnF (InL e) _) = case e of
        ELit l -> pretty l
        EIdent i -> pretty i
        EIf c t -> pif <+> c <+> pthen <+> t
        EIfElse c t e -> pif <+> c <+> pthen <+> t <+> pelse <+> e
        _ -> pretty "fixme"
