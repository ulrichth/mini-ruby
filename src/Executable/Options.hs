--------------------------------------------------------------------------------
-- ...
--------------------------------------------------------------------------------

module Executable.Options (
  Options(..),
  defaultOptions,
  optDescrs
) where

import System.Console.GetOpt (ArgDescr(..),OptDescr(..),usageInfo)
import System.Exit           (exitSuccess)
import System.IO             (hFlush,stdout)

import Executable.Messages

--------------------------------------------------------------------------------
-- Option type                                                                --
--------------------------------------------------------------------------------

data Options = Options
  { optCompileOnlyMode :: Bool
  , optRawMode         :: Bool
  , optVerboseMode     :: Bool
  , optDebugMode       :: Bool
  , optExpression      :: Maybe String
  , optConfigFile      :: FilePath
  , optOutputDir       :: Maybe FilePath
  } deriving Show

--------------------------------------------------------------------------------
-- Default values                                                             --
--------------------------------------------------------------------------------

defaultOptions :: Options
defaultOptions = Options
  { optCompileOnlyMode = False
  , optRawMode         = False
  , optVerboseMode     = False
  , optDebugMode       = False
  , optExpression      = Nothing
  , optConfigFile      = "config.json"
  , optOutputDir       = Nothing
  }

--------------------------------------------------------------------------------
-- Option descriptions and actions                                            --
--------------------------------------------------------------------------------

optDescrs :: String -> [OptDescr (Options -> IO Options)]
optDescrs version =
  [ Option "c" [] (NoArg (\opts ->
      return opts {optCompileOnlyMode = True}))
      "compile-only mode (stop after compiling the level code)"
  -- , Option "i" [] (NoArg (\opts -> do
  --     putStrLn
  --       "run: Message:\n\
  --       \  Interactive mode is not yet implemented :-("
  --     return opts {optInteractive = True}))
  --     "interactive mode (run a REPL-like interactive mode)"
  , Option "r" [] (NoArg (\opts ->
      return opts {optRawMode = True}))
      "raw mode (do not use markup in warning and error messages)"
  , Option "v" [] (NoArg (\opts ->
      return opts {optVerboseMode = True}))
      "verbose mode (print all compilation and execution messages)"
  , Option "d" [] (NoArg (\opts ->
      return opts {optDebugMode = True}))
      "debug mode (insert debug information into level code)"
  , Option "e" [] (ReqArg (\str opts ->
      return opts {optExpression = Just str}) "<expression>")
      "dummy"
  , Option "l" [] (ReqArg (\str opts ->
      return opts {optConfigFile = str}) "<filePath>")
      "dummy"
  , Option "o" [] (ReqArg (\str opts ->
      return opts {optOutputDir = Just str}) "<dirPath>")
      "dummy"
  , Option ""  ["help"] (NoArg (\_ -> do
      putStr $ usageInfo helpHeader $ optDescrs version
      hFlush stdout
      exitSuccess))
      "print this help message"
  , Option ""  ["version"] (NoArg (\_ -> do
      putStrLn $ "Version: "++version
      exitSuccess))
      "print the version number"
  ]

--------------------------------------------------------------------------------
