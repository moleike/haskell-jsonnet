{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Jsonnet.Parser where

import Control.Applicative hiding (many, some)
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Combinators.Expr
import qualified Control.Monad.Combinators.NonEmpty as NE
import Control.Monad.Except
import Data.Char
import Data.Fix
import Data.Either
import Data.Functor
import Data.Functor.Sum
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import GHC.IO.Exception hiding (IOError)
import Language.Jsonnet.Annotate
import Language.Jsonnet.Common
import Language.Jsonnet.Parser.SrcSpan
import Language.Jsonnet.Syntax
import Language.Jsonnet.Syntax.Annotated
import System.Directory
import System.FilePath.Posix (takeDirectory)
import System.IO.Error (tryIOError)
import Text.Megaparsec hiding (ParseError, parse, sepBy1)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data ParseError
  = ParseError (ParseErrorBundle Text Void)
  | ImportError IOError (Maybe SrcSpan)
  deriving (Eq, Show)

parse ::
  MonadError ParseError m =>
  FilePath ->
  Text ->
  m Expr'
parse fp inp =
  liftEither
    $ left ParseError
    $ runParser (sc *> exprP <* eof) fp inp

resolveImports ::
  (MonadError ParseError m, MonadIO m) =>
  FilePath ->
  Expr' ->
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
      liftEither $ left (flip ImportError (Just a)) inp
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
  end <- getSourcePos
  pure $ AnnF res $ SrcSpan begin end

identifier :: Parser String
identifier = do
  ident <- p
  when (ident `elem` reservedKeywords)
    $ fail
    $ "Keyword " <> ident <> " cannot be an identifier."
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
  char '\\'
  choice
    [ char '\"' $> '\"',
      char '\'' $> '\'', -- this is specific to jsonnet
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
  string "\\u"
  hex <- ("0x" ++) <$> count 4 hexDigitChar
  pure (chr $ read hex)

verbatimString :: Parser String
verbatimString = char '@' *> (quoted (char '\'') <|> quoted (char '\"'))
  where
    quoted c =
      c
        *> manyTill
          ((c *> c) <|> anySingle)
          (try $ c <* notFollowedBy c)

unquoted :: Parser Expr'
unquoted = Fix <$> annotateLoc (mkStrF <$> identifier)

stringP :: Parser Expr'
stringP = Fix <$> annotateLoc (mkStrF <$> (verbatimString <|> stringLiteral))

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
      expr <- exprP
      pure $ mkAssertF cond msg expr

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
paramsP = parens (param `sepBy` comma)
  where
    param = (,) <$> identifier <*> optional (symbol "=" *> exprP)

function ::
  Parser [Param Expr'] ->
  Parser Expr' ->
  Parser Expr'
function ps expr = Fix <$> annotateLoc (mkFunF <$> ps <*> expr)

functionP :: Parser Expr'
functionP = keywordP "function" *> function paramsP exprP

binding = do
  name <- identifier
  _ <- symbol "="
  expr <- exprP
  pure (name, expr)

localFunc = do
  name <- identifier
  ps <- paramsP
  _ <- symbol "="
  expr <- function (pure ps) exprP
  pure (name, expr)

localP :: Parser Expr'
localP = Fix <$> annotateLoc localExpr
  where
    localExpr = do
      _ <- keywordP "local"
      bnds <- (try binding <|> localFunc) `NE.sepBy1` comma
      _ <- symbol ";"
      expr <- exprP
      pure $ mkLocalF bnds expr

arrayP :: Parser Expr'
arrayP = Fix <$> annotateLoc array
  where
    array = mkArrayF <$> brackets (exprP `sepEndBy` comma)

--a[start:stop:step]
objectP :: Parser Expr'
objectP = Fix <$> annotateLoc object
  where
    object = do
      xs <- braces (eitherP localP fieldP `sepEndBy` comma)
      let (ls, fs) = (lefts xs, rights xs)
      pure $ mkObjectF fs ls
    fieldP = try methodP <|> pairP
    pairP = do
      k <- keyP
      h <- sepP
      v <- exprP
      pure $ Field k v h
    keyP = brackets exprP <|> unquoted <|> stringP
    methodP = do
      k <- unquoted
      ps <- paramsP
      h <- sepP
      v <- function (pure ps) exprP
      pure $ Field k v h
    sepP = try (symbol "::" $> True) <|> (colon $> False)
    localP = do
      _ <- keywordP "local"
      try binding <|> localFunc

importP :: Parser Expr'
importP = Fix <$> annotateLoc importDecl
  where
    importDecl = mkImportF <$> (keywordP "import" *> stringLiteral)

binary ::
  Text ->
  (Expr' -> Expr' -> Expr') ->
  Operator Parser Expr'
binary name f = InfixL (f <$ (operator name))
  where
    operator sym = try $ symbol sym <* notFollowedBy opChar
    opChar = oneOf ("!$:~+-&|^=<>*/%" :: [Char]) <?> "operator"

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
    [ binary "*" (mkBinOp (Arith Mul)),
      binary "/" (mkBinOp (Arith Div)),
      binary "%" (mkBinOp (Arith Mod))
    ],
    [ binary "+" (mkBinOp (Arith Add)),
      binary "-" (mkBinOp (Arith Sub))
    ],
    [ binary ">>" (mkBinOp (Bitwise ShiftR)),
      binary "<<" (mkBinOp (Bitwise ShiftL))
    ],
    [ binary "in" (mkBinOp In),
      binary ">" (mkBinOp (Comp Gt)),
      binary "<=" (mkBinOp (Comp Le)),
      binary ">=" (mkBinOp (Comp Ge)),
      binary "<" (mkBinOp (Comp Lt))
    ],
    [ binary "==" (mkBinOp (Comp Eq)),
      binary "!=" (mkBinOp (Comp Ne))
    ],
    [binary "&" (mkBinOp (Bitwise And))],
    [binary "^" (mkBinOp (Bitwise Xor))],
    [binary "|" (mkBinOp (Bitwise Or))],
    [binary "&&" (mkBinOp (Logical LAnd))],
    [binary "||" (mkBinOp (Logical LOr))]
  ]

-- | application, indexing and lookup: e(...) e[...] e.f
-- all have the same precedence (the highest)
postfixOperators :: Parser (Expr' -> Expr')
postfixOperators =
  foldr1 (flip (.))
    <$> some
      ( apply
          <|> try slice
          <|> index
          <|> lookup
      )
  where
    apply = flip mkApply <$> argsP
    index = flip mkIndex <$> brackets exprP
    lookup = flip mkLookup <$> (symbol "." *> unquoted)
    slice = brackets $ do
      start <- optional exprP <* colon
      end <- optional exprP <* colon
      step <- optional exprP
      pure $ mkSlice start end step

argsP :: Parser (Args Expr')
argsP = try (parens named) <|> parens posal
  where
    posal = Positional <$> (exprP `sepBy` comma)
    named = Named <$> (args `sepBy` comma)
    args = (,) <$> identifier <*> (symbol "=" *> exprP)

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
