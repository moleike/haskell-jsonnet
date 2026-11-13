{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- |
Module                  : Language.Jsonnet.Pretty
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable
-}
module Language.Jsonnet.Pretty where

import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text qualified as JSON (encodeToLazyText)
import Data.Bifunctor (bimap, first)
import Data.Bool (bool)
import Data.Fix
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific (..))
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import Data.Vector qualified as V
import GHC.IO.Exception (IOException (..))
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Language.Jsonnet.Syntax.Annotated qualified as Ann
import Prettyprinter
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Pos

(<$$>) :: Doc ann -> Doc ann -> Doc ann
(<$$>) = \x y -> vcat [x, y]

-- reserved keywords
pnull
    , ptrue
    , pfalse
    , pif
    , pthen
    , pelse
    , pimport
    , pimportstr
    , pimportbin
    , perror
    , plocal
    , pfunction
    , passert
    , pfor
    , pin ::
        Doc ann
pnull = "null"
ptrue = "true"
pfalse = "false"
pif = "if"
pthen = "then"
pelse = "else"
pimport = "import"
pimportstr = "importstr"
pimportbin = "importbin"
perror = "error"
plocal = "local"
pfunction = "function"
passert = "assert"
pfor = "for"
pin = "in"

prettyName :: Name a -> Doc ann
prettyName = pretty

ppNumber :: Scientific -> Doc ann
ppNumber s
    | e < 0 || e > 1024 =
        pretty $
            toLazyText $
                scientificBuilder s
    | otherwise = pretty (coefficient s * 10 ^ e)
  where
    e = base10Exponent s

ppJsonIndent :: Int -> JSON.Value -> Doc ann
ppJsonIndent indentLevel = ppJson False indentLevel

ppJsonCompact :: JSON.Value -> Doc ann
ppJsonCompact = ppJson True 0

ppJson :: Bool -> Int -> JSON.Value -> Doc ann
ppJson isCompact i = \case
    JSON.Null -> pnull
    JSON.Number n -> ppNumber n
    JSON.Bool True -> ptrue
    JSON.Bool False -> pfalse
    JSON.String s -> ppString (Key.fromText s)
    JSON.Array a -> ppArray a
    JSON.Object o -> ppObject o
  where
    encloseSep' l r ds = case ds of
        [] -> if isCompact then l <> space <> r else l <> r
        _ ->
            if isCompact
                then l <> hcat (punctuate (comma <> space) ds) <> r
                else l <$$> indent i (vcat $ punctuate comma ds) <$$> r

    ppObject o = encloseSep' lbrace rbrace xs
      where
        prop (k, v) = ppString k <> colon <+> ppJson isCompact i v
        xs = map prop (sortOn fst $ KeyMap.toList o)

    ppArray a = encloseSep' lbracket rbracket xs
      where
        xs = map (ppJson isCompact i) (V.toList a)

    ppString = pretty . JSON.encodeToLazyText

prettySrcSpan :: SrcSpan -> Doc ann
prettySrcSpan SrcSpan{spanBegin, spanEnd} =
    pretty (sourceName spanBegin)
        <> colon
        <> lc spanBegin spanEnd
  where
    lc (SourcePos _ lb cb) (SourcePos _ le ce)
        | lb == le =
            pretty (unPos lb)
                <> colon
                <> pretty (unPos cb)
                <> dash
                <> pretty (unPos ce)
        | otherwise =
            pretty (unPos lb)
                <> colon
                <> pretty (unPos cb)
                <> dash
                <> pretty (unPos le)
                <> colon
                <> pretty (unPos ce)
    dash = pretty '-'

prettyParserError :: ParserError -> Doc ann
prettyParserError (ParseError e) = pretty (errorBundlePretty e)

prettyCheckError :: CheckError -> Doc ann
prettyCheckError = \case
    DuplicateParam e ->
        "duplicate parameter"
            <+> squotes (pretty e)
    DuplicateBinding e ->
        "duplicate local var"
            <+> squotes (pretty e)
    PosAfterNamedParam ->
        "positional after named argument"

prettyEvalError :: EvalError -> Doc ann
prettyEvalError = \case
    TypeMismatch expected actual ->
        "type mismatch:"
            <+> "expected"
            <+> pretty expected
            <+> "but got"
            <+> pretty actual
    InvalidKey k ->
        "invalid key:"
            <+> pretty k
    DuplicateKey k ->
        "duplicate key:"
            <+> pretty k
    InvalidIndex k ->
        "invalid index:"
            <+> pretty k
    NoSuchKey k ->
        "no such key:"
            <+> pretty k
    IndexOutOfBounds i ->
        "index out of bounds:"
            <+> ppNumber i
    DivByZero ->
        "divide by zero exception"
    VarNotFound v ->
        "variable"
            <+> squotes (pretty $ show v)
            <+> "is not defined"
    UnboundSelf ->
        "can't use self outside of an object."
    UnboundSuper ->
        "can't use super outside of an object."
    NoSuperClass ->
        "attempt to use super when there is no super class."
    ExtVarNotFound v ->
        "external variable"
            <+> squotes (pretty v)
            <+> "is not defined"
    AssertionFailed e ->
        "assertion failed:" <+> pretty e
    StdError e -> pretty e
    RuntimeError e -> pretty e
    ParamNotBound s ->
        "parameter not bound:"
            <+> pretty (show s)
    BadParam s ->
        "function has no parameter"
            <+> squotes (pretty s)
    ManifestError e ->
        "manifest error:"
            <+> pretty e
    TooManyArgs n ->
        "too many args, function has"
            <+> pretty n
            <+> "parameter(s)"
    UnresolvedImport f (IOError _ _ _ desc _ _) ->
        "import error:"
            <+> pretty desc
            <+> squotes (pretty f)
    InfiniteRecursion ->
        "infinite recursion detected"

prettyStackFrame :: StackFrame a -> Doc ann
prettyStackFrame StackFrame{name, span = span'} =
    prettySrcSpan span' <+> f name
  where
    f :: Text -> Doc ann
    f "top-level" = mempty
    f x = "function" <+> angles (pretty x)

prettyBacktrace :: Backtrace a -> Doc ann
prettyBacktrace (Backtrace xs) = vcat $ prettyStackFrame <$> xs

prettyError :: Error -> Doc ann
prettyError = \case
    EvalError e bt ->
        "Runtime error:"
            <+> prettyEvalError e
                <$$> indent 2 (prettyBacktrace bt)
    ParserError e -> prettyParserError e
    CheckError e sp ->
        "Static error:"
            <+> prettyCheckError e
                <$$> indent 2 (maybe mempty prettySrcSpan sp)

prettyVisibility :: Visibility -> Doc ann
prettyVisibility = \case
    Visible -> ":"
    Hidden -> "::"
    Forced -> ":::"

commaSep :: [Doc ann] -> Doc ann
commaSep = concatWith (surround comma)

bracketSep :: Doc ann -> [Doc ann] -> Doc ann
bracketSep = encloseSep lbracket rbracket

parensSep :: Doc ann -> [Doc ann] -> Doc ann
parensSep = encloseSep lparen rparen

braceSep :: Doc ann -> [Doc ann] -> Doc ann
braceSep = encloseSep lbrace rbrace

ppField :: EField (Doc ann) -> Doc ann
ppField EField{..} =
    surround (ppOverride <> prettyVisibility visibility) key' value
  where
    ppOverride = bool mempty "+" override
    key' = bool key (brackets key) computed

ppObject :: forall ann. [(Ident, Doc ann)] -> [Assert (Doc ann)] -> [Doc ann] -> Doc ann
ppObject l asserts o =
    encloseSep
        lbrace
        rbrace
        comma
        (mcons (ppLocal' l) (asserts' <> o))
  where
    mcons :: Maybe a -> [a] -> [a]
    mcons ma as = maybe as (: as) ma

    asserts' :: [Doc ann]
    asserts' = map prettyAssert asserts

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
    Compl -> "~"
    LNot -> "!"
    Plus -> "+"
    Minus -> "-"
    Err -> "<prim:err>"

prettyBinOp :: BinOp -> Doc ann
prettyBinOp = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
    Lt -> "<"
    Le -> "<="
    Gt -> ">"
    Ge -> ">="
    Eq -> "=="
    Ne -> "!="
    And -> "&"
    Or -> "|"
    Xor -> "^"
    ShiftL -> "<<"
    ShiftR -> ">>"
    LAnd -> "&&"
    LOr -> "||"
    In -> "in"
    Lookup -> "<prim:lookup>"

ppFun :: [Param (Doc ann)] -> Doc ann -> Doc ann
ppFun ps e = pfunction <> parens (commaSep ps') <+> e
  where
    f = maybe mempty (equals <>)
    ps' = uncurry (<>) . bimap pretty f <$> ps

ppMaybeDoc :: Maybe (Doc ann) -> Doc ann
ppMaybeDoc = fromMaybe mempty

prettyImport :: FilePath -> Import -> Doc ann
prettyImport fp = \case
    Code -> pimport <+> squotes (pretty fp)
    Str -> pimportstr <+> squotes (pretty fp)
    Bin -> pimportbin <+> squotes (pretty fp)

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

prettyAssert :: Assert (Doc ann) -> Doc ann
prettyAssert (Assert c m) = passert <+> c <> ppMaybeDoc ((colon <>) <$> m)

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
    EObj ls o asserts -> ppObject ls asserts (ppField <$> o)
    ELocal xs e -> ppLocal (NE.toList xs) (parens e)
    EErr a -> perror <+> parens a
    EAssert assert e -> prettyAssert assert <> semi <+> e
    EIndex a b -> parens a <> brackets b
    ELookup a i -> enclose a (pretty i) dot
    EUnyOp o a -> parens (prettyUnyOp o <> a)
    EBinOp o a b -> parens (parens a <+> prettyBinOp o <+> parens b)
    ESlice a b e s -> parens a <> bracketSep colon (ppMaybeDoc <$> [b, e, s])
    EArrComp e cs -> lbracket <> e <+> ppCompSpec cs <> rbracket
    EObjComp f ls cs -> ppObject ls [] [ppField f <+> ppCompSpec cs]
    EImport it fp -> prettyImport fp it
