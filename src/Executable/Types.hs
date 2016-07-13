{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

--------------------------------------------------------------------------------
-- ...
--------------------------------------------------------------------------------

module Executable.Types (

  RunResult,
  runIO,
  runIOWithIssueHandler,

  RunError(errorSource,errorData),
  lexerError,
  parserError,
  analyzerError,
  printerError,
  interpreterError,
  compilerError,
  runtimeError,
  otherError,

  RunWarning(warningSource,warningData),
  lexerWarning,
  parserWarning,
  analyzerWarning,
  printerWarning,
  interpreterWarning,
  compilerWarning,
  runtimeWarning,
  otherWarning,
  addWarning,
  addWarnings,
  runWithWarnings,

  Source(..),
  Pos,

  -- TranslationMode(..),
  Command,
  ExecutionContext,
  OutputFragment(..),
  isTextFragment,
  isWarningFragment,
  isErrorFragment,
  fromTextFragment,
  fromWarningFragment,
  fromErrorFragment,

  LanguageData(..)
) where

import Control.Applicative       ((<*>),(<$>),pure)
import Control.Monad.Error       (Error(..),ErrorT,liftIO,runErrorT,throwError)
import Control.Monad.Trans.Error (ErrorList(..))
import Control.Monad.Writer      (MonadWriter,WriterT,censor,listen,runWriterT
                                 ,tell)
import Data.Aeson                (FromJSON(..),Value(..),(.:),(.:?),withArray
                                 ,withObject,withText)
import Data.Aeson.Types          (Parser)
import Data.Vector               (toList)
import System.Exit               (ExitCode)

--------------------------------------------------------------------------------
-- General result type, errors and warnings                                   --
--------------------------------------------------------------------------------

type RunResult = ErrorT [RunError] (WriterT [RunWarning] IO)

runIO :: RunResult a -> IO a
runIO action = do
  (Right value,_) <- runWriterT $ runErrorT action
  return value

runIOWithIssueHandler
  :: RunResult b
  -> ([RunWarning] -> [RunError] -> IO b)
  -> IO b
runIOWithIssueHandler action errorHandler = do
  (res,warnings) <- runWriterT $ runErrorT action
  case res of
    Left  errs  -> errorHandler warnings errs
    Right value -> return value

--------------------------------------------------------------------------------

data RunError = RunError
  { errorSource :: Source
  , errorData   :: Either String (Pos,String)
  } deriving (Eq,Show)

instance Error RunError where
  strMsg = error

instance ErrorList RunError where
  listMsg = error

newError :: Source -> Either String (Pos,String) -> RunError
newError = RunError

lexerError :: (Pos,String) -> RunError
lexerError = newError Lexer . Right

parserError :: (Pos,String) -> RunError
parserError = newError Parser . Right

analyzerError :: (Pos,String) -> RunError
analyzerError = newError Analyzer . Right

printerError :: (Pos,String) -> RunError
printerError = newError Printer . Right

interpreterError :: Either String (Pos,String) -> RunError
interpreterError = newError Interpreter

compilerError :: Either String (Pos,String) -> RunError
compilerError = newError Compiler

runtimeError :: Either String (Pos,String) -> RunError
runtimeError = newError Runtime

otherError :: String -> RunError
otherError = newError Other . Left

--------------------------------------------------------------------------------

data RunWarning = RunWarning
  { warningSource :: Source
  , warningData   :: Either String (Pos,String)
  } deriving (Eq,Show)

newWarning :: Source -> Either String (Pos,String) -> RunWarning
newWarning = RunWarning

lexerWarning :: (Pos,String) -> RunWarning
lexerWarning = newWarning Lexer . Right

parserWarning :: (Pos,String) -> RunWarning
parserWarning = newWarning Parser . Right

analyzerWarning :: (Pos,String) -> RunWarning
analyzerWarning = newWarning Analyzer . Right

printerWarning :: (Pos,String) -> RunWarning
printerWarning = newWarning Printer . Right

interpreterWarning :: Either String (Pos,String) -> RunWarning
interpreterWarning = newWarning Interpreter

compilerWarning :: Either String (Pos,String) -> RunWarning
compilerWarning = newWarning Compiler

runtimeWarning :: Either String (Pos,String) -> RunWarning
runtimeWarning = newWarning Runtime

otherWarning :: String -> RunWarning
otherWarning = newWarning Other . Left

addWarning :: (MonadWriter [RunWarning] m) => RunWarning -> m ()
addWarning = tell . (:[])

addWarnings :: (MonadWriter [RunWarning] m) => [RunWarning] -> m ()
addWarnings = tell

runWithWarnings :: (MonadWriter [RunWarning] m) => m a -> m (a,[RunWarning])
runWithWarnings = censor (const []) . listen

--------------------------------------------------------------------------------

data Source
  = Lexer
  | Parser
  | Analyzer
  | Printer
  | Interpreter
  | Compiler
  | Runtime
  | Other
  deriving (Eq,Show)

type Pos = (Int,Int,Int)

--------------------------------------------------------------------------------
-- Translating execution output                                               --
--------------------------------------------------------------------------------

-- data TranslationMode
--   = AtRuntime
--   | OnTermination

type Command = (String,[String])
type ExecutionContext = (Command,[Command],[FilePath])

data OutputFragment
  = TextFragment    String
  | WarningFragment RunWarning
  | ErrorFragment   RunError

isTextFragment :: OutputFragment -> Bool
isTextFragment (TextFragment _) = True
isTextFragment _                = False

isWarningFragment :: OutputFragment -> Bool
isWarningFragment (WarningFragment _) = True
isWarningFragment _                   = False

isErrorFragment :: OutputFragment -> Bool
isErrorFragment (ErrorFragment _) = True
isErrorFragment _                 = False

fromTextFragment :: OutputFragment -> String
fromTextFragment (TextFragment text) = text

fromWarningFragment :: OutputFragment -> RunWarning
fromWarningFragment (WarningFragment text) = text

fromErrorFragment :: OutputFragment -> RunError
fromErrorFragment (ErrorFragment text) = text

--------------------------------------------------------------------------------
-- Language data                                                              --
--------------------------------------------------------------------------------

data LanguageData = LanguageData
  {name                     :: String
  ,levels                   :: [String]
  ,objectCodeFileType       :: String
  ,executionCommandPatterns :: [String]
  } deriving (Eq,Show)

instance FromJSON LanguageData where
  parseJSON (Object o) = LanguageData
    <$> o   .:  "name"
    <*> ((o .:  "levels") >>= parseArray parseLevel)
    <*> o   .:  "objectCodeFileType"
    <*> ((o .:  "executionCommandPatterns") >>= parseJSON)
    where
      parseArray :: (Value -> Parser a) -> Value -> Parser [a]
      parseArray elemParser = withArray  "" $ mapM elemParser . toList
      parseLevel :: Value -> Parser String
      parseLevel = withObject "" (.: "name")

--------------------------------------------------------------------------------
