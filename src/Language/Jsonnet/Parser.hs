{- |
Module                  : Language.Jsonnet.Parser
Copyright               : (c) 2020-2021 Alexandre Moreno
SPDX-License-Identifier : BSD-3-Clause OR Apache-2.0
Maintainer              : Alexandre Moreno <alexmorenocano@gmail.com>
Stability               : experimental
Portability             : non-portable

Parser for Jsonnet source code.

-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.Jsonnet.Parser
  ( parse,
    resolveImports,
  )
where

import Control.Applicative hiding (many, some)
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Combinators.Expr
import qualified Control.Monad.Combinators.NonEmpty as NE
import Control.Monad.Except
import Data.Char
import Data.Either
import Data.Fix
import Data.Functor
import Data.Functor.Sum
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import GHC.IO.Exception hiding (IOError)
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Error
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Language.Jsonnet.Syntax.Annotated
import System.Directory
import System.FilePath.Posix (takeDirectory)
import System.IO.Error (tryIOError)
import Text.Megaparsec hiding (ParseError, parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parse ::
  MonadError Error m =>
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  Text ->
  -- | AST with unresolved imports
  m Expr'
parse fp inp =
  liftEither $
    left (ParserError . ParseError) $
      runParser (sc *> exprP <* eof) fp inp

resolveImports ::
  (MonadError Error m, MonadIO m) =>
  -- | File path (modules are resolved relative to this path)
  FilePath ->
  -- | AST with unresolved imports
  Expr' ->
  -- | AST with imports resolved
  m Expr
resolveImports fp = foldFixM go
  where
    go (AnnF (InL e) a) = pure $ Fix $ AnnF e a
    go (AnnF (InR (Const (Import fp'))) a) =
      resolveImports fp'
        =<< parse fp'
        =<< readImportFile fp' a
    readImportFile fp' a = do
      inp <- readFile' fp'
      liftEither $ left (ParserError . flip ImportError (Just a)) inp
      where
        readFile' =
          liftIO
            . tryIOError
            . withCurrentDirectory (takeDirectory fp)
            . T.readFile

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//" <|> L.skipLineComment "#"
    blockComment = L.skipBlockComment "/*" "*/"

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

annotateLoc :: Parser (f a) -> Parser (AnnF f SrcSpan a)
annotateLoc p = do
  begin <- getSourcePos
  res <- p
  AnnF res . SrcSpan begin <$> getSourcePos

identifier :: Parser String
identifier = do
  ident <- p
  when (ident `elem` reservedKeywords) $
    fail $
      "Keyword " <> ident <> " cannot be an identifier."
  pure ident
  where
    p =
      lexeme
        ( (:)
            <$> (letterChar <|> char '_')
            <*> many (alphaNumChar <|> char '_')
        )

keywordP :: Text -> Parser Text
keywordP keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

-- unfinished string parser
stringLiteral :: Parser String
stringLiteral = quoted (char '\"') <|> quoted (char '\'')
  where
    quoted c =
      c
        *> manyTill
          ( try escapeUnicode <|> escapeAscii
              <|> anySingle
          )
          c

escapeAscii :: Parser Char
escapeAscii = do
  void (char '\\')
  choice
    [ char '\"' $> '\"',
      char '\'' $> '\'', -- this one is jsonnet specific
      char '\\' $> '\\',
      char '/' $> '/',
      char 'n' $> '\n',
      char 'r' $> '\r',
      char 'f' $> '\f',
      char 't' $> '\t',
      char 'b' $> '\b'
    ]

escapeUnicode :: Parser Char
escapeUnicode = do
  _ <- string "\\u"
  hex <- ("0x" ++) <$> count 4 hexDigitChar
  pure (chr $ read hex)

verbatimString :: Parser String
verbatimString = char '@' *> (quoted (char '\'') <|> quoted (char '\"'))
  where
    quoted c =
      c
        *> manyTill
          (c *> c <|> anySingle)
          (try $ c <* notFollowedBy c)

textBlock :: Parser String
textBlock = do
  _ <- symbol "|||" <* sc'
  ref <- L.indentLevel
  x <- line
  xs <-
    manyTill
      (L.indentGuard (sc'' ref) EQ ref >> line)
      (try $ sc' *> symbol "|||")
  pure $ concat (x : xs)
  where
    line :: Parser String
    line = (++) <$> many (anySingleBut '\n') <*> some (char '\n')
    sc' :: Parser ()
    sc' = L.space (void $ some (char ' ' <|> char '\t')) empty empty
    sc'' :: Pos -> Parser ()
    sc'' x = void $ count' 0 (unPos x - 1) (oneOf [' ', '\t'])

unquoted :: Parser Expr'
unquoted = Fix <$> annotateLoc (mkStrF <$> identifier)

stringP :: Parser Expr'
stringP =
  Fix
    <$> annotateLoc
      ( mkStrF
          <$> ( verbatimString
                  <|> stringLiteral
                  <|> textBlock
              )
      )

numberP :: Parser Expr'
numberP = Fix <$> annotateLoc number
  where
    number = mkFloatF <$> lexeme L.scientific

identP :: Parser Expr'
identP = Fix <$> annotateLoc (mkIdentF <$> (try (T.unpack <$> symbol "$") <|> identifier))

booleanP :: Parser Expr'
booleanP = Fix <$> annotateLoc boolean
  where
    boolean =
      keywordP "true" $> mkBoolF True
        <|> keywordP "false" $> mkBoolF False

nullP :: Parser Expr'
nullP = Fix <$> annotateLoc null
  where
    null = keywordP "null" $> mkNullF

errorP :: Parser Expr'
errorP = Fix <$> annotateLoc error
  where
    error = keywordP "error" *> (mkErrorF <$> exprP)

assertP :: Parser Expr'
assertP = Fix <$> annotateLoc assert
  where
    assert = do
      cond <- keywordP "assert" *> exprP
      msg <- optional (colon *> exprP)
      _ <- symbol ";"
      mkAssertF cond msg <$> exprP

ifElseP :: Parser Expr'
ifElseP = Fix <$> annotateLoc ifElseExpr
  where
    ifElseExpr = do
      cond <- keywordP "if" *> exprP
      expr <- keywordP "then" *> exprP
      option
        (mkIfF cond expr)
        (keywordP "else" *> (mkIfElseF cond expr <$> exprP))

paramsP :: Parser [Param Expr']
paramsP = parens (param `sepEndBy` comma)
  where
    param = (,) <$> identifier <*> optional (symbol "=" *> exprP)

function ::
  Parser [Param Expr'] ->
  Parser Expr' ->
  Parser Expr'
function ps expr = Fix <$> annotateLoc (mkFunF <$> ps <*> expr)

functionP :: Parser Expr'
functionP = keywordP "function" *> function paramsP exprP

forspecP :: Parser (CompSpec Expr')
forspecP = do
  _ <- keywordP "for"
  var <- identifier
  _ <- keywordP "in"
  forspec <- exprP
  ifspec <- optional (keywordP "if" *> exprP)
  pure CompSpec {..}

binding :: Parser (String, Expr')
binding = do
  name <- identifier
  _ <- symbol "="
  expr <- exprP
  pure (name, expr)

localFunc :: Parser (String, Expr')
localFunc = do
  name <- identifier
  ps <- paramsP
  _ <- symbol "="
  expr <- function (pure ps) exprP
  pure (name, expr)

localBndsP :: Parser (NonEmpty (String, Expr'))
localBndsP = do
  _ <- keywordP "local"
  (try binding <|> localFunc) `NE.sepBy1` comma

localP :: Parser Expr'
localP = Fix <$> annotateLoc localExpr
  where
    localExpr = do
      bnds <- localBndsP
      _ <- symbol ";"
      mkLocalF bnds <$> exprP

arrayP :: Parser Expr'
arrayP = Fix <$> annotateLoc (brackets (try arrayComp <|> array))
  where
    array = mkArrayF <$> (exprP `sepEndBy` comma)
    arrayComp = do
      expr <- exprP <* optional comma
      comps <- NE.some forspecP
      return $ mkArrCompF expr comps

objectP :: Parser Expr'
objectP = Fix <$> annotateLoc (braces (try objectComp <|> object))
  where
    object = do
      xs <- eitherP localP fieldP `sepEndBy` comma
      let (ls, fs) = (lefts xs, rights xs)
      pure $ mkObjectF fs ls
    fieldP = try methodP <|> pairP
    pairP = do
      key <- keyP
      (override, visibility) <-
        (,)
          <$> option False (symbol "+" $> True) <*> sepP
      value <- exprP
      pure $ EField {..}
    keyP = brackets exprP <|> unquoted <|> stringP
    methodP = do
      let override = False
      key <- unquoted
      ps <- paramsP
      visibility <- sepP
      value <- function (pure ps) exprP
      pure $ EField {..}
    sepP =
      try (symbol ":::" $> Forced)
        <|> try (symbol "::" $> Hidden)
        <|> symbol ":" $> Visible
    localP = do
      _ <- keywordP "local"
      try binding <|> localFunc
    objectComp = do
      locals1 <- localP `sepEndBy` comma
      expr <- pairP <* optional comma
      locals2 <- localP `sepEndBy` comma
      comps <- NE.some forspecP
      return $ mkObjCompF expr (locals1 <> locals2) comps

importP :: Parser Expr'
importP = Fix <$> annotateLoc importDecl
  where
    importDecl = mkImportF <$> (keywordP "import" *> stringLiteral)

binary ::
  Text ->
  (Expr' -> Expr' -> Expr') ->
  Operator Parser Expr'
binary name f = InfixL (f <$ operator name)
  where
    operator sym = try $ symbol sym <* notFollowedBy opChar
    opChar = oneOf (":~+&|^=<>*/%" :: String) <?> "operator"

prefix ::
  Text ->
  (Expr' -> Expr') ->
  Operator Parser Expr'
prefix name f = Prefix (f <$ symbol name)

-- | associativity and operator precedence
--  1. @e(...)@ @e[...]@ @e.f@ (application and indexing)
--  2. @+@ @-@ @!@ @~@ (the unary operators)
--  3. @*@ @/@ @%@ (these, and the remainder below, are binary operators)
--  4. @+@ @-@
--  5. @<<@ @>>@
--  6. @<@ @>@ @<=@ @>=@ @in@
--  7. @==@ @!=@
--  8. @&@
--  9. @^@
-- 10. @|@
-- 11. @&&@
-- 12. @||@
-- default is associate to the left
opTable :: [[Operator Parser Expr']]
opTable =
  [ [Postfix postfixOperators],
    [ prefix "+" (mkUnyOp Plus),
      prefix "-" (mkUnyOp Minus),
      prefix "!" (mkUnyOp LNot),
      prefix "~" (mkUnyOp Compl)
    ],
    [ binary "*" (mkBinOp Mul),
      binary "/" (mkBinOp Div),
      binary "%" (mkBinOp Mod)
    ],
    [ binary "+" (mkBinOp Add),
      binary "-" (mkBinOp Sub),
      Postfix postfixObjectMerge
    ],
    [ binary ">>" (mkBinOp ShiftR),
      binary "<<" (mkBinOp ShiftL)
    ],
    [ binary "in" (mkBinOp In),
      binary ">" (mkBinOp Gt),
      binary "<=" (mkBinOp Le),
      binary ">=" (mkBinOp Ge),
      binary "<" (mkBinOp Lt)
    ],
    [ binary "==" (mkBinOp Eq),
      binary "!=" (mkBinOp Ne)
    ],
    [binary "&" (mkBinOp And)],
    [binary "^" (mkBinOp Xor)],
    [binary "|" (mkBinOp Or)],
    [binary "&&" (mkBinOp LAnd)],
    [binary "||" (mkBinOp LOr)]
  ]

-- | shorthand syntax for object composition:
-- when the right-hand side is an object literal the '+'
-- operator can be elided.
postfixObjectMerge :: Parser (Expr' -> Expr')
postfixObjectMerge = flip (mkBinOp Add) <$> objectP

-- | application, indexing and lookup: e(...) e[...] e.f
-- all have the same precedence (the highest)
postfixOperators :: Parser (Expr' -> Expr')
postfixOperators =
  foldr1 (flip (.))
    <$> some
      ( applyP
          <|> try sliceP
          <|> indexP
          <|> lookupP
      )

indexP :: Parser (Expr' -> Expr')
indexP = flip mkIndex <$> brackets exprP

lookupP :: Parser (Expr' -> Expr')
lookupP = flip mkLookup <$> (symbol "." *> unquoted)

-- arguments are many postional followed by many named
-- just like Python
applyP :: Parser (Expr' -> Expr')
applyP = flip mkApply <$> argsP
  where
    argsP :: Parser (Args Expr')
    argsP = Args <$> parens (args `sepEndBy` comma) <*> tailstrict
      where
        args = try named <|> posal
        posal = Pos <$> exprP
        named = Named <$> identifier <*> (symbol "=" *> exprP)
        tailstrict = option Lazy (keywordP "tailstrict" $> Strict)

sliceP :: Parser (Expr' -> Expr')
sliceP = brackets $ do
  start <- optional exprP <* colon
  end <- optional exprP
  step <- optional (colon *> optional exprP)
  pure $ mkSlice start end (join step)

primP :: Parser Expr'
primP =
  lexeme $
    choice
      [ try identP,
        numberP,
        stringP,
        booleanP,
        nullP,
        ifElseP,
        functionP,
        objectP,
        arrayP,
        localP,
        importP,
        errorP,
        assertP,
        parens exprP
      ]

exprP :: Parser Expr'
exprP = makeExprParser primP opTable

reservedKeywords :: [String]
reservedKeywords =
  [ "assert",
    "else",
    "error",
    "false",
    "for",
    "function",
    "if",
    "import",
    "importstr",
    "in",
    "local",
    "null",
    "tailstrict",
    "then",
    "true"
  ]
