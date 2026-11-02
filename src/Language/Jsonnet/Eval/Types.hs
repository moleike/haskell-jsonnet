{-# LANGUAGE TemplateHaskell #-}
-- | 

module Language.Jsonnet.Eval.Types where

import Control.Lens (makeLenses)
import Data.ByteString (ByteString)
import Data.IORef ( IORef )
import Data.Map ( Map )
import Language.Jsonnet.Common (Import)
import Language.Jsonnet.Core (Core)
import Language.Jsonnet.Parser.SrcSpan (SrcSpan)
import Unbound.Generics.LocallyNameless ( Name )
import {-# SOURCE #-} Language.Jsonnet.Value (Value)

type Env = Map (Name Core) Value

-- | Simulate a call-stack to report stack traces
data CallStack = CallStack
    { _spans :: [Maybe SrcSpan]
    -- ^ source location of call-sites
    , _scopes :: [Name Core]
    -- ^ names of called functions
    }

makeLenses ''CallStack

data ImportResult = ImportResult
    { foundAt :: FilePath
    , content :: ByteString
    }

newtype Importer = Importer
    {runImporter :: FilePath -> String -> IO ImportResult}

data ImpKey = ImpKey !Import !FilePath deriving (Eq, Ord)

data EvalState = EvalState
    { _env :: Env
    -- ^ binding local variables to their values
    , _baseEnv :: Env
    -- ^ pristine context with only std
    , _callStack :: CallStack
    -- ^ call-stack simulation
    , _currentPos :: Maybe SrcSpan
    -- ^ source span of expression being evaluated
    , _gen :: IORef Integer
    -- ^ fresh names
    , _currentFile :: FilePath
    -- ^ current file being evaluated
    , _loadedModules :: IORef (Map ImpKey Value)
    -- ^ memoization of imports
    , _importer :: Importer
    -- ^ runs the IO for resolving paths
    }

makeLenses ''EvalState

