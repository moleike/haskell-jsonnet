{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as LC (putStrLn)
import Data.Maybe
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
-- import Language.Jsonnet.Value
import Options.Applicative
import Paths_jsonnet (version)
import Prettyprinter (Pretty, pretty)

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

mkConfig :: Options -> IO Config
mkConfig Options {..} = do
  let fname = case input of
        Stdin -> ""
        FileInput path -> path
        ExecInput _ -> ""
  pure Config {..}

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
  pure Options {..}

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
    input :: Input
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
