{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as LC (putStrLn)
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Language.Jsonnet
import Language.Jsonnet.Annotate
import Language.Jsonnet.Desugar
import Language.Jsonnet.Error
import Language.Jsonnet.Eval
import Language.Jsonnet.Value (ExtVars(..))
import Options.Applicative
import Paths_jsonnet (version)
import Prettyprinter (Pretty, pretty)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import Data.Bifunctor (first, second, bimap)
import Data.Void (Void)
import System.Environment (lookupEnv)
import System.Exit (die)

main :: IO ()
main = do
  runProgram =<< execParser options
  return ()

runProgram ::
  -- |
  Options ->
  IO ()
runProgram opts@Options {..} = do
  src <- readSource input
  conf <- mkConfig opts
  outp <- interpret conf src
  either printError (printResult output format) outp

printResult :: Output -> Format -> JSON.Value -> IO ()
printResult outp Json val =
  writeOutput (JSON.encode val) outp
printResult outp Plaintext (JSON.String s) =
  writeOutput (encodeToLazyByteString s) outp
  where
    encodeToLazyByteString =
      toLazyByteString . encodeUtf8Builder
printResult _ Plaintext _ =
  print "Runtime error: expected string result"

writeOutput :: L.ByteString -> Output -> IO ()
writeOutput bs = \case
  FileOutput path -> L.writeFile path bs
  Stdout -> LC.putStrLn bs

printError :: Error -> IO ()
printError = print . pretty

readSource :: Input -> IO Text
readSource = \case
  Stdin -> TIO.getContents
  FileInput path -> TIO.readFile path
  ExecInput str -> pure (T.pack str)

mkExtVar :: (Text, Either ExtVarType ExtVar) -> IO (Text, ExtVar)
mkExtVar = \case
  (s, Right extVar) -> pure (s, extVar)
  (envVar, Left extVarType) ->
    lookupEnv (T.unpack envVar) >>= \case
      Just extVar -> pure $ (envVar, ExtVar extVarType (T.pack extVar))
      Nothing -> die "No env variable"

mkConfig :: Options -> IO Config
mkConfig Options {..} = do
  let fname = case input of
        Stdin -> ""
        FileInput path -> path
        ExecInput _ -> ""
  exts <- traverse mkExtVar extVars
  extVars' <- constructExtVars exts
  pure Config { fname = fname, extVars = extVars' }

fileOutput :: Parser Output
fileOutput =
  fromMaybe Stdout
    <$> ( optional $
            FileOutput
              <$> strOption
                ( long "output-file"
                    <> short 'o'
                    <> metavar "<filename>"
                    <> help "Write to the output file rather than stdout"
                )
        )

fileInput :: Parser (Maybe String)
fileInput =
  optional $
    strArgument
      ( metavar "<filename>"
          <> help "Jsonnet source file or stdin"
      )

parseOpts :: Parser Options
parseOpts = do
  input <- mkInput <$> exec <*> fileInput
  output <- fileOutput
  format <-
    flag
      Json
      Plaintext
      ( long "string"
          <> short 'S'
          <> help "Expect a string, manifest as plain text"
      )
  extStrs <- parseExtStr
  extCodes <- parseExtCode
  pure Options { extVars = extStrs <> extCodes, .. }

parseExtStr :: Parser [(Text, Either ExtVarType ExtVar)]
parseExtStr =
  many $
    option
      (second (interpretAs ExtStr) <$> extVarParser)
      ( long "ext-str" <> help "External string variable" )

parseExtCode :: Parser [(Text, Either ExtVarType ExtVar)]
parseExtCode =
  many $
    option
      (second (interpretAs ExtCode) <$> extVarParser)
      ( long "ext-code" <> help "External code variable" )

interpretAs :: ExtVarType -> Maybe Text -> Either ExtVarType ExtVar
interpretAs t = \case
  Nothing -> Left t
  Just s -> Right $ ExtVar t s

extVarParser :: ReadM (Text, Maybe Text)
extVarParser = eitherReader f
  where
    f = first MP.errorBundlePretty . MP.runParser (MP.try pair <|> single) ""

    single :: MP.Parsec Void String (Text, Maybe Text)
    single = (, Nothing) . T.pack <$> MP.someTill MPC.asciiChar MP.eof

    pair :: MP.Parsec Void String (Text, Maybe Text)
    pair = bimap T.pack (Just . T.pack) <$> liftA2 (,)
      (MP.someTill MPC.asciiChar (MPC.char '='))
      (MP.someTill MPC.asciiChar MP.eof)

mkInput :: Bool -> Maybe String -> Input
mkInput exec = \case
  Nothing -> Stdin
  Just e | exec -> ExecInput e
  Just p | otherwise -> FileInput p

exec :: Parser Bool
exec =
  switch
    ( long "exec"
        <> short 'e'
        <> help "Treat filename as code"
    )

ver :: Parser (a -> a)
ver =
  infoOption
    verStr
    ( long "version"
        <> short 'v'
        <> help "Print version of the program"
    )
  where
    verStr = "Jsonnet command line " <> showVersion version

options :: ParserInfo Options
options =
  info
    (ver <*> helper <*> parseOpts)
    ( fullDesc
        <> noIntersperse
    )

data Options = Options
  { output :: Output,
    format :: Format,
    input :: Input,
    extVars :: [(Text, Either ExtVarType ExtVar)]
    -- ^ ExtVarType determines the interpretation of the environment variable
  }

data Input
  = FileInput FilePath
  | ExecInput String
  | Stdin
  deriving (Eq, Show)

data Output
  = FileOutput FilePath
  | Stdout
  deriving (Eq, Show)

data Format = Json | Plaintext
  deriving (Eq, Show)
