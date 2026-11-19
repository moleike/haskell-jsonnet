{-# LANGUAGE TemplateHaskell #-}

module Language.Jsonnet.Eval.Types where

import Control.Lens (makeLenses)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Primitive.SmallArray
import Language.Jsonnet.Common (Import, Name)
import Language.Jsonnet.Parser.SrcSpan (SrcSpan)
import {-# SOURCE #-} Language.Jsonnet.Value (Value)

data Env = Env
    { _vars :: ![SmallArray Value]
    , _self :: !(Maybe Value)
    , _super :: !(Maybe Value)
    , _std :: !Value
    }

makeLenses ''Env

-- | Simulate a call-stack to report stack traces
data CallStack = CallStack
    { _spans :: ![Maybe SrcSpan]
    -- ^ source location of call-sites
    , _scopes :: ![Name]
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
    , _callStack :: !(IORef CallStack)
    -- ^ call-stack simulation
    , _currentPos :: !(IORef (Maybe SrcSpan))
    -- ^ source span of expression being evaluated
    , _currentFile :: !FilePath
    -- ^ current file being evaluated
    , _loadedModules :: !(IORef (Map ImpKey Value))
    -- ^ memoization of imports
    , _importer :: !Importer
    -- ^ runs the IO for resolving paths
    }

makeLenses ''EvalState
