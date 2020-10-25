{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Jsonnet where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.Map.Strict (singleton)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Jsonnet.Core
import qualified Language.Jsonnet.Desugar as Desugar
import Language.Jsonnet.Error
import qualified Language.Jsonnet.Eval as Eval
import Language.Jsonnet.Eval (Eval, EvalState (..), Thunk (..))
import qualified Language.Jsonnet.Parser as Parser
import Language.Jsonnet.Pretty ()
import Language.Jsonnet.Std
import Language.Jsonnet.Syntax.Annotated
import Text.PrettyPrint.ANSI.Leijen (displayS, pretty, renderCompact)

type JsonnetM = ReaderT Config (ExceptT Error IO)

data Config = Config
  { fname :: FilePath
  }
  deriving (Eq, Show)

jsonnet :: Config -> Text -> IO (Either Error Text)
jsonnet conf = runJsonnetM conf . (interpret >=> render)

runJsonnetM :: Config -> JsonnetM a -> IO (Either Error a)
runJsonnetM conf = runExceptT . flip runReaderT conf

interpret :: Text -> JsonnetM JSON.Value
interpret = parse >=> desugar >=> evaluate

parse :: Text -> JsonnetM Expr
parse inp =
  asks fname >>= lift . withExceptT ParserError . go
  where
    go fp =
      Parser.resolveImports fp
        =<< Parser.parse fp inp

desugar :: Expr -> JsonnetM Core
desugar = pure . Desugar.desugar

runEval :: EvalState -> Eval a -> ExceptT Error IO a
runEval st = withExceptT mkErr . Eval.runEval st
  where
    mkErr (e, EvalState {curSpan}) = EvalError e curSpan

-- evaluate a Core expression with the implicit stdlib
evaluate :: Core -> JsonnetM JSON.Value
evaluate = lift . runEval evalSt . (Eval.eval >=> Eval.manifest)
  where
    stdlib = singleton "std" (Thunk $ pure std)
    evalSt = EvalState {ctx = stdlib, curSpan = Nothing}

render :: JSON.Value -> JsonnetM Text
render = pure . T.pack . show . pretty

renderC :: JSON.Value -> JsonnetM Text
renderC a = pure $ T.pack $ displayS (renderCompact (pretty a)) ""
