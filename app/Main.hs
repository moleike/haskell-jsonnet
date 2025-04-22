{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad (when)
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as JSON.Key
import Data.Aeson.KeyMap qualified as JSON.KeyMap
import Data.Bifunctor (first, second)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as LC
import Data.Functor (void)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import Data.Void (Void)
import Language.Jsonnet
import Language.Jsonnet.Error
import Language.Jsonnet.Pretty (ppJson, prettyError)
import Options.Applicative hiding (str)
import Paths_jsonnet (version)
import System.Directory qualified as Dir
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

main :: IO ()
main = do
  runProgram =<< execParser options
  return ()

runProgram ::
  Options ->
  IO ()
runProgram opts@Options {..} = do
  src <- readSource input
  conf <- mkConfig opts
  outp <- interpret conf src
  either printError (printResult output outputMode format) outp

writeFileMulti :: FilePath -> L.ByteString -> IO ()
writeFileMulti outFile content = do
  -- We shouldn't touch the file if it exists and has the same content
  fileExists <- Dir.doesFileExist outFile
  if fileExists
    then do
      existingContent <- L.readFile outFile
      when (existingContent /= content) $
        L.writeFile outFile content
    else L.writeFile outFile content
  putStrLn outFile

printResult :: Output -> OutputMode -> Format -> JSON.Value -> IO ()
printResult outp outputMode format val =
  case format of
    Json -> case outp of
      FileOutput path -> L.writeFile path (encode val)
      FileOutputMulti outDir -> case val of
        JSON.Object obj -> do
          Dir.createDirectoryIfMissing True outDir
          void $ flip JSON.KeyMap.traverseWithKey obj $ \fileName output ->
            writeFileMulti (outDir </> JSON.Key.toString fileName) (encode output)
        _ -> die "Runtime error: result must be a JSON object in multi mode"
      Stdout -> LC.putStrLn (encode val)
    Plaintext -> case val of
      JSON.String s -> case outp of
        FileOutput path -> L.writeFile path (encodeToLazyByteString s)
        FileOutputMulti _ -> die "Runtime error: multi mode can only be used with JSON output"
        Stdout -> LC.putStrLn (encodeToLazyByteString s)
      _ -> die "Runtime error: result must be a string for plaintext output"
  where
    encode :: JSON.Value -> L.ByteString
    encode = case outputMode of
      Pretty -> encodeToLazyByteString . T.pack . show . ppJson 4
      Compact -> JSON.encode

    encodeToLazyByteString :: Text -> L.ByteString
    encodeToLazyByteString = toLazyByteString . encodeUtf8Builder

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
  fromMaybe Stdout <$> optional fileOpts
  where
    fileOpts :: Parser Output
    fileOpts =
      FileOutput
        <$> strOption
          ( long "output-file"
              <> short 'o'
              <> metavar "<filename>"
              <> help "Write to the output file rather than stdout"
          )
          <|> FileOutputMulti
        <$> strOption
          ( long "multi"
              <> short 'm'
              <> metavar "<dir>"
              <> help "Write multiple files to the directory, list files on stdout"
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
  | FileOutputMulti FilePath
  | Stdout
  deriving (Eq, Show)

data OutputMode
  = Pretty
  | Compact
  deriving (Eq, Show)

data Format = Json | Plaintext
  deriving (Eq, Show)
