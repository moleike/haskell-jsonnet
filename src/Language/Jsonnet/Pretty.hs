{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Jsonnet.Pretty where

import qualified Data.HashMap.Lazy as H
import Data.Text (unpack)
import qualified Data.Vector as V
import GHC.IO.Exception (IOException (..))
import Language.Jsonnet.Error
import Language.Jsonnet.JSON
import Language.Jsonnet.Parser (ParseError (..))
import Language.Jsonnet.Parser.SrcSpan
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Pos
import Text.PrettyPrint.ANSI.Leijen hiding (encloseSep)
import Unbound.Generics.LocallyNameless (Name, name2String)
import Data.Scientific (toRealFloat)

instance Pretty (Name a) where
  pretty v = pretty (name2String v)

encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep l r s ds = case ds of
  [] -> l <> r
  [d] -> l <> d <> r
  _ -> l <> line <> indent 2 (cat (punctuate s ds)) <> line <> r

instance Pretty JSON where
  pretty =
    \case
      JNull -> text "null"
      JNum a -> double (toRealFloat a)
      JBool True -> text "true"
      JBool False -> text "false"
      JStr a -> dquotes (text (unpack a))
      JArr a -> encloseSep lbracket rbracket comma $ map pretty (V.toList a)
      JObj a -> encloseSep lbrace rbrace comma $ map prop (H.toList a)
        where
          prop (k, v) = dquotes (text (unpack k)) <> colon <+> pretty v

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
