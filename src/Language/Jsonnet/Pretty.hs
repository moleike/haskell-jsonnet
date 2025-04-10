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
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Text as JSON (encodeToLazyText)
import Data.Bifunctor (bimap, first)
import Data.Bool (bool)
import Data.Fix
import Data.Functor.Sum
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Scientific (Scientific (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import qualified Data.Vector as V
import GHC.IO.Exception (IOException (..))
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import qualified Language.Jsonnet.Syntax.Annotated as Ann
import Prettyprinter
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Pos
import Unbound.Generics.LocallyNameless (Name, name2String)

(<$$>) :: Doc ann -> Doc ann -> Doc ann
(<$$>) = \x y -> vcat [x, y]

-- reserved keywords
pnull, ptrue, pfalse, pif, pthen, pelse, pimport, pimportstr, pimportbin,
  perror, plocal, pfunction, passert, pfor, pin :: Doc ann
pnull = pretty "null"
ptrue = pretty "true"
pfalse = pretty "false"
pif = pretty "if"
pthen = pretty "then"
pelse = pretty "else"
pimport = pretty "import"
pimportstr = pretty "importstr"
pimportbin = pretty "importbin"
perror = pretty "error"
plocal = pretty "local"
pfunction = pretty "function"
passert = pretty "assert"
pfor = pretty "for"
pin = pretty "in"

prettyName :: Name a -> Doc ann
prettyName = pretty . name2String

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
  JSON.String s -> ppString (Key.fromText s)
  JSON.Array a -> ppArray a
  JSON.Object o -> ppObject' o
  where
    encloseSep' l r s ds = case ds of
      [] -> l <> r
      _ -> l <$$> indent i (vcat $ punctuate s ds) <$$> r
    ppObject' o = encloseSep' lbrace rbrace comma xs
      where
        prop (k, v) = ppString k <> colon <+> ppJson i v
        xs = map prop (sortOn fst $ KeyMap.toList o)
    ppArray a = encloseSep' lbracket rbracket comma xs
      where
        xs = map (ppJson i) (V.toList a)
    ppString = pretty . LT.unpack . JSON.encodeToLazyText

prettySrcSpan :: SrcSpan -> Doc ann
prettySrcSpan SrcSpan {spanBegin, spanEnd} =
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

prettyParserError :: ParserError -> Doc ann
prettyParserError (ParseError e) = pretty (errorBundlePretty e)
prettyParserError (ImportError (IOError _ _ _ desc _ f) sp) =
  pretty "Parse error:"
    <+> pretty f
    <+> parens (pretty desc)
    <$$> indent 4 (maybe mempty prettySrcSpan sp)

prettyCheckError :: CheckError -> Doc ann
prettyCheckError = \case
  DuplicateParam e ->
    pretty "duplicate parameter"
      <+> squotes (pretty e)
  DuplicateBinding e ->
    pretty "duplicate local var"
      <+> squotes (pretty e)
  PosAfterNamedParam ->
    pretty "positional after named argument"

prettyEvalError :: EvalError -> Doc ann
prettyEvalError = \case
  TypeMismatch expected actual ->
    pretty "type mismatch:"
      <+> pretty "expected"
      <+> pretty (T.unpack expected)
      <+> pretty "but got"
      <+> pretty (T.unpack actual)
  InvalidKey k ->
    pretty "invalid key:"
      <+> pretty k
  DuplicateKey k ->
    pretty "duplicate key:"
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
  ExtVarNotFound v ->
    pretty "external variable"
      <+> squotes (pretty v)
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

prettyStackFrame :: StackFrame a -> Doc ann
prettyStackFrame StackFrame {name, span = span'} =
  prettySrcSpan span' <+> f (name2String name)
  where
    f "top-level" = mempty
    f x = pretty "function" <+> angles (pretty x)

prettyBacktrace :: Backtrace a -> Doc ann
prettyBacktrace (Backtrace xs) = vcat $ prettyStackFrame <$> xs

prettyError :: Error -> Doc ann
prettyError = \case
  EvalError e bt ->
    pretty "Runtime error:"
      <+> prettyEvalError e
      <$$> indent 2 (prettyBacktrace bt)
  ParserError e -> prettyParserError e
  CheckError e sp ->
    pretty "Static error:"
      <+> prettyCheckError e
      <$$> indent 2 (maybe mempty prettySrcSpan sp)

prettyVisibility :: Visibility -> Doc ann
prettyVisibility = \case
  Visible -> pretty ":"
  Hidden -> pretty "::"
  Forced -> pretty ":::"

commaSep :: [Doc ann] -> Doc ann
commaSep = concatWith (surround comma)

bracketSep :: Doc ann -> [Doc ann] -> Doc ann
bracketSep = encloseSep lbracket rbracket

parensSep :: Doc ann -> [Doc ann] -> Doc ann
parensSep = encloseSep lparen rparen

braceSep :: Doc ann -> [Doc ann] -> Doc ann
braceSep = encloseSep lbrace rbrace

ppField :: EField (Doc ann) -> Doc ann
ppField EField {..} =
  surround (ppOverride <> prettyVisibility visibility) key' value
  where
    ppOverride = pretty (bool "" "+" override)
    key' = bool key (brackets key) computed

ppObject :: [(Ident, Doc ann)] -> [Doc ann] -> Doc ann
ppObject l o =
  encloseSep
    lbrace
    rbrace
    comma
    (mcons (ppLocal' l) o)
  where
    mcons ma as = maybe as (: as) ma
    ppLocal' :: [(Ident, Doc ann)] -> Maybe (Doc ann)
    ppLocal' [] = Nothing
    ppLocal' xs =
      Just $ commaSep ((plocal <+>) . uncurry (surround equals) <$> xs')
      where
        xs' = first pretty <$> xs

ppLocal :: [(Ident, Doc ann)] -> Doc ann -> Doc ann
ppLocal xs e =
  plocal
    <+> commaSep (uncurry (surround equals) <$> xs')
      <> semi
    <+> e
  where
    xs' = first pretty <$> xs

prettyUnyOp :: UnyOp -> Doc ann
prettyUnyOp = \case
  Compl -> pretty "~"
  LNot -> pretty "!"
  Plus -> pretty "+"
  Minus -> pretty "-"
  Err -> pretty "<prim:err>"

prettyBinOp :: BinOp -> Doc ann
prettyBinOp = \case
  Add -> pretty "+"
  Sub -> pretty "-"
  Mul -> pretty "*"
  Div -> pretty "/"
  Mod -> pretty "%"
  Lt -> pretty "<"
  Le -> pretty "<="
  Gt -> pretty ">"
  Ge -> pretty ">="
  Eq -> pretty "=="
  Ne -> pretty "!="
  And -> pretty "&"
  Or -> pretty "|"
  Xor -> pretty "^"
  ShiftL -> pretty "<<"
  ShiftR -> pretty ">>"
  LAnd -> pretty "&&"
  LOr -> pretty "||"
  In -> pretty "in"
  Lookup -> pretty "<prim:lookup>"

ppFun :: [Param (Doc ann)] -> Doc ann -> Doc ann
ppFun ps e = pfunction <> parens (commaSep ps') <+> e
  where
    f = maybe mempty (equals <>)
    ps' = uncurry (<>) . bimap pretty f <$> ps

ppMaybeDoc :: Maybe (Doc ann) -> Doc ann
ppMaybeDoc = fromMaybe mempty

prettyExpr' :: Ann.Expr' -> Doc ann
prettyExpr' = prettyFixExprF' . forget'

prettyImport :: Import -> Doc ann
prettyImport = \case
  Import fp -> pimport <+> squotes (pretty fp)
  Importstr fp -> pimportstr <+> squotes (pretty fp)
  Importbin fp -> pimportbin <+> squotes (pretty fp)

prettyFixExprF' :: Fix ExprF' -> Doc ann
prettyFixExprF' = foldFix $ \case
  InR (Const import') -> prettyImport import'
  InL e -> ppExpr e

prettyFixExprF :: Fix ExprF -> Doc ann
prettyFixExprF = foldFix ppExpr

prettyExpr :: Ann.Expr -> Doc ann
prettyExpr = prettyFixExprF . forget'

ppArgs :: Args (Doc ann) -> Doc ann
ppArgs (Args args _) = parensSep comma (ppArg <$> args)

ppArg :: Arg (Doc ann) -> Doc ann
ppArg (Pos a) = a
ppArg (Named n a) = surround equals (pretty n) a

ppCompSpec :: NonEmpty (CompSpec (Doc ann)) -> Doc ann
ppCompSpec cs = hsep $ f <$> NE.toList cs
  where
    f (CompSpec v f' ifspecs) =
      pfor
        <+> pretty v
        <+> pin
        <+> parens f'
        <+> hsep (map (pif <+>) ifspecs)

ppExpr :: ExprF (Doc ann) -> Doc ann
ppExpr = \case
  ENull -> pnull
  EBool True -> ptrue
  EBool False -> pfalse
  EStr s -> squotes (pretty s)
  ENum n -> ppNumber n
  EFun ps e -> ppFun ps e
  EApply a args -> parens a <> ppArgs args
  EIdent i -> pretty i
  EIf c t -> pif <+> c <+> pthen <+> parens t
  EIfElse c t e -> pif <+> c <+> pthen <+> parens t <+> pelse <+> parens e
  EArr a -> bracketSep comma a
  EObj ls o -> ppObject ls (ppField <$> o)
  ELocal xs e -> ppLocal (NE.toList xs) (parens e)
  EErr a -> perror <+> parens a
  EAssert (Assert c m e) -> passert <+> c <> ppMaybeDoc ((colon <>) <$> m) <> semi <+> e
  EIndex a b -> parens a <> brackets b
  ELookup a i -> enclose a (pretty i) dot
  EUnyOp o a -> parens (prettyUnyOp o <> a)
  EBinOp o a b -> parens (parens a <+> prettyBinOp o <+> parens b)
  ESlice a b e s -> parens a <> bracketSep colon (ppMaybeDoc <$> [b, e, s])
  EArrComp e cs -> lbracket <> e <+> ppCompSpec cs <> rbracket
  EObjComp f ls cs -> ppObject ls [ppField f <+> ppCompSpec cs]
