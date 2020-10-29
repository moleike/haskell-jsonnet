{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Jsonnet.Pretty where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Lazy as H
import Data.List (sortOn)
import Data.Scientific (Scientific (..))
import Data.Text (unpack)
import qualified Data.Text.Lazy as LT (unpack)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import qualified Data.Vector as V
import GHC.IO.Exception (IOException (..))
import Language.Jsonnet.Error
import Language.Jsonnet.Parser (ParseError (..))
import Language.Jsonnet.Parser.SrcSpan
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Pos
import Text.PrettyPrint.ANSI.Leijen hiding (encloseSep)
import Unbound.Generics.LocallyNameless (Name, name2String)

instance Pretty (Name a) where
  pretty v = pretty (name2String v)

ppJson :: Int -> JSON.Value -> Doc
ppJson i =
  \case
    JSON.Null -> text "null"
    JSON.Number n -> ppNumber n
    JSON.Bool True -> text "true"
    JSON.Bool False -> text "false"
    JSON.String s -> dquotes (text (unpack s))
    JSON.Array a -> ppArray a
    JSON.Object o -> ppObject o
  where
    encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
    encloseSep l r s ds = case ds of
      [] -> l <> r
      _ -> l <$$> indent i (vcat $ punctuate s ds) <$$> r
    ppObject :: JSON.Object -> Doc
    ppObject o = encloseSep lbrace rbrace comma xs
      where
        prop (k, v) = dquotes (text (unpack k)) <> colon <+> ppJson i v
        xs = map prop (sortOn fst $ H.toList o)
    ppArray :: JSON.Array -> Doc
    ppArray a = encloseSep lbracket rbracket comma xs
      where
        xs = map (ppJson i) (V.toList a)
    ppNumber :: Scientific -> Doc
    ppNumber s
      | e < 0 || e > 1024 =
        text
          $ LT.unpack
          $ toLazyText
          $ scientificBuilder s
      | otherwise = integer (coefficient s * 10 ^ e)
      where
        e = base10Exponent s

instance Pretty JSON.Value where
  pretty = ppJson 4

instance Pretty SrcSpan where
  pretty (SrcSpan {spanBegin, spanEnd}) =
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

instance Pretty ParseError where
  pretty (ParseError e) = pretty (errorBundlePretty e)
  pretty (ImportError (IOError _ _ _ desc _ f) sp) =
    text "Parse error:"
      <+> pretty f
      <+> parens (text desc)

instance Pretty ManifestError where
  pretty (NotAJsonValue e) =
    text "Not a JSON value:"
      <+> text (unpack e)

instance Pretty EvalError where
  pretty =
    \case
      TypeMismatch {..} ->
        text "Type mismatch:"
          <+> text "expected"
          <+> text (unpack expected)
          <+> text "but got"
          <+> text (unpack actual)
      InvalidKey k ->
        text "Invalid key:"
          <+> text (unpack k)
      NoSuchKey k ->
        text "No such key:"
          <+> text (unpack k)
      IndexOutOfBounds i ->
        text "Index out of bounds:"
          <+> (int i)
      DivByZero ->
        text "Divide by zero exception"
      VarNotFound v ->
        text "Variable"
          <+> squotes (text $ unpack v)
          <+> text "is not defined"
      ManifestError e ->
        text "Manifest error:"
          <+> pretty e
      AssertionFailed e ->
        text "Assertion failed:"
          <+> e
      StdError e -> e
      RuntimeError e ->
        text "Runtime error:"
          <+> text (unpack e)

instance Pretty Error where
  pretty =
    \case
      EvalError e sp -> pretty e <$$> indent 4 (pretty sp)
      ParserError e -> pretty e
