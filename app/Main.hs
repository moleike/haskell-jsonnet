{-# LANGUAGE ApplicativeDo #-}

module Main where

import qualified Data.Aeson as JSON
import Data.Bifunctor (first, second)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as LC (putStrLn)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Data.Void (Void)
import Language.Jsonnet
import Language.Jsonnet.Error
import Language.Jsonnet.Pretty (ppJson, prettyError)
import Options.Applicative hiding (str)
import Paths_jsonnet (version)
import System.Environment (lookupEnv)
import System.Exit (die)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC

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
  either printError (printResult output outputMode format) outp

encodeToLazyByteString :: Text -> L.ByteString
encodeToLazyByteString = toLazyByteString . encodeUtf8Builder

printResult :: Output -> OutputMode -> Format -> JSON.Value -> IO ()
printResult outp outputMode Json val =
  writeOutput (encode val) outp
  where
    encode = case outputMode of
      Pretty -> encodeToLazyByteString . T.pack . show . ppJson 4
      Compact -> JSON.encode
printResult outp _ Plaintext (JSON.String s) =
  writeOutput (encodeToLazyByteString s) outp
printResult _ _ Plaintext _ =
  print "Runtime error: expected string result"

writeOutput :: L.ByteString -> Output -> IO ()
writeOutput bs = \case
  FileOutput path -> L.writeFile path bs
  Stdout -> LC.putStrLn bs

printError :: Error -> IO ()
printError = print . prettyError

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
      Just extVar -> pure $ (envVar, ExtVar extVarType (Inline (T.pack extVar)))
      Nothing -> die "No env variable"

mkConfig :: Options -> IO Config
mkConfig Options {..} = do
  let fname = case input of
        Stdin -> ""
        FileInput path -> path
        ExecInput _ -> ""
  extVars' <- constructExtVars =<< traverse mkExtVar extVars
  pure Config {fname = fname, extVars = extVars'}

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
  outputMode <-
    flag
      Pretty
      Compact
      ( long "compact"
          <> help "Produce a compact JSON output"
      )
  format <-
    flag
      Json
      Plaintext
      ( long "string"
          <> short 'S'
          <> help "Expect a string, manifest as plain text"
      )
  extStrs <- parseExtStr
  extStrFiles <- parseExtStrFile
  extCodes <- parseExtCode
  extCodeFiles <- parseExtCodeFile
  pure Options {extVars = extStrs <> extStrFiles <> extCodes <> extCodeFiles, ..}

parseExtStr :: Parser [(Text, Either ExtVarType ExtVar)]
parseExtStr =
  many $
    option
      (second (interpretAs ExtStr) <$> extVarParser)
      ( long "ext-str"
          <> short 'V'
          <> metavar "VAR=VALUE"
          <> help "External string variable"
      )

parseExtStrFile :: Parser [(Text, Either ExtVarType ExtVar)]
parseExtStrFile =
  many $
    option
      (second (Right . ExtVar ExtStr) <$> extVarFileParser)
      ( long "ext-str-file"
          <> help "External string variable as file"
          <> metavar "FILE"
          <> action "file"
      )

parseExtCode :: Parser [(Text, Either ExtVarType ExtVar)]
parseExtCode =
  many $
    option
      (second (interpretAs ExtCode) <$> extVarParser)
      ( long "ext-code"
          <> metavar "VAR=EXPR"
          <> help "External code variable"
      )

parseExtCodeFile :: Parser [(Text, Either ExtVarType ExtVar)]
parseExtCodeFile =
  many $
    option
      (second (Right . ExtVar ExtCode) <$> extVarFileParser)
      ( long "ext-code-file"
          <> help "External code variable as file"
          <> metavar "FILE"
          <> action "file"
      )

interpretAs :: ExtVarType -> Maybe ExtVarContent -> Either ExtVarType ExtVar
interpretAs t = \case
  Nothing -> Left t
  Just s -> Right $ ExtVar t s

extVarParser :: ReadM (Text, Maybe ExtVarContent)
extVarParser = eitherReader f
  where
    f = first MP.errorBundlePretty . MP.runParser (MP.try pair <|> envVar) ""

    pair :: MP.Parsec Void String (Text, Maybe ExtVarContent)
    pair = second (Just . Inline . T.pack) <$> parsePair

    envVar :: MP.Parsec Void String (Text, Maybe ExtVarContent)
    envVar = (,Nothing) . T.pack <$> MP.someTill MPC.asciiChar MP.eof

extVarFileParser :: ReadM (Text, ExtVarContent)
extVarFileParser = eitherReader f
  where
    f = first MP.errorBundlePretty . MP.runParser (second File <$> parsePair) ""

parsePair :: MP.Parsec Void String (Text, String)
parsePair =
  first T.pack
    <$> liftA2
      (,)
      (MP.someTill MPC.asciiChar (MPC.char '='))
      (MP.someTill MPC.asciiChar MP.eof)

mkInput :: Bool -> Maybe String -> Input
mkInput exec' = \case
  Nothing -> Stdin
  Just e | exec' -> ExecInput e
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
    outputMode :: OutputMode,
    format :: Format,
    input :: Input,
    -- | ExtVarType determines the interpretation of the environment variable
    extVars :: [(Text, Either ExtVarType ExtVar)]
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

data OutputMode
  = Pretty
  | Compact
  deriving (Eq, Show)

data Format = Json | Plaintext
  deriving (Eq, Show)
