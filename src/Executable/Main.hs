--------------------------------------------------------------------------------
-- ...                                                                        --
--------------------------------------------------------------------------------

module Main where

import qualified Data.ByteString.Lazy as B (readFile)

import Control.Concurrent    (MVar,forkIO,isEmptyMVar,newEmptyMVar,putMVar
                             ,takeMVar)
import Control.Exception     (finally)
import Control.Monad.Error   (catchError,liftIO,throwError,when)
import Control.Monad.Writer  (pass)
import Data.Aeson            (decode)
import Data.Foldable         (foldrM)
import Data.List             (intercalate,splitAt)
import Data.Maybe            (fromJust,isJust,isNothing)
import System.Console.GetOpt (ArgOrder(..),getOpt)
import System.Directory      (createDirectoryIfMissing,doesDirectoryExist
                             ,doesFileExist,getCurrentDirectory
                             ,removeDirectoryRecursive,setCurrentDirectory)
import System.Environment    (getArgs)
import System.Exit           (ExitCode(..),exitSuccess,exitWith)
import System.FilePath       ((<.>),(</>),dropExtension,takeFileName
                             ,splitFileName,splitPath)
import System.IO             (BufferMode(..),Handle,hFlush,hGetContents,hPutStr
                             ,hPutStrLn,hSetBuffering,stderr,stdin,stdout)
import System.IO.Temp        (withTempDirectory)
import System.Process        (runInteractiveCommand,waitForProcess)
import Text.Read             (readMaybe)

import Executable.Messages
import Executable.Options
import Executable.Types
import Lang.Interface

--------------------------------------------------------------------------------

version :: String
version = "0.1.0.1"

--------------------------------------------------------------------------------
-- Main function and dispatcher                                               --
--------------------------------------------------------------------------------

-- | The main function!
main :: IO ()
main = dispatcher

-- | The 'dispatcher' function reads the command line arguments, parses and
-- processes them, calls the 'run' function, attaches the 'issueHandler' and
-- exits with the computed exit code.
dispatcher :: IO ()
dispatcher = do
  -- disable buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  -- read command line arguments
  args <- getArgs
  -- read run data from command line arguments
  runData <- readRunData args
  -- run executable with temporary directory
  let workingDir = getWorkingDir runData
  exitCode <- withTempDirectory workingDir "tmp." $ \tempDir -> do
    -- run the compilation and execution phases with issue handling
    let runData' = runData {getTempDirName = last $ splitPath tempDir}
    exitCode <- runIOWithIssueHandler (run runData') (issueHandler runData')
    -- return the final exit code
    return exitCode
  exitWith exitCode

--------------------------------------------------------------------------------
-- Run data                                                                   --
--------------------------------------------------------------------------------

data RunData = RunData
  { getCompileOnlyMode :: Bool
  , getRawMode         :: Bool
  , getVerboseMode     :: Bool
  , getDebugMode       :: Bool
  , getExpression      :: Maybe String
  , getOutputDir       :: Maybe FilePath
  , getLanguageData    :: LanguageData
  , getLevel           :: Int
  , getLevelCode       :: String
  , getWorkingDir      :: FilePath
  , getInputFileName   :: FilePath
  , getTempDirName     :: FilePath
  } deriving Show

readRunData :: [String] -> IO RunData
readRunData args = case getOpt Permute (optDescrs version) args of
  (actions,nonOpts,[]) -> do
    -- get option values and perform associated IO actions if given
    opts <- foldl (>>=) (return defaultOptions) actions
    -- check if level and level code file are given
    when (not $ length nonOpts == 2) $
      exitWithNativeError False wrongUsageError
    -- get level and code and check if the level is valid
    let [levelStr,inputFile] = nonOpts
    let mLevel = readMaybe levelStr
    when (isNothing mLevel) $
      exitWithNativeError False $ levelNotValidError levelStr
    let level = fromJust mLevel
    -- read level code and language data
    let (workingDir,inputFileName) = splitFileName inputFile
    levelCode <- readLevelCode    (optVerboseMode opts) inputFile
    langData  <- readLanguageData (optVerboseMode opts) (optConfigFile opts)
    -- return RunData object
    return $ RunData
      { getCompileOnlyMode = optCompileOnlyMode opts
      , getRawMode         = optRawMode opts
      , getVerboseMode     = optVerboseMode opts
      , getDebugMode       = optDebugMode opts
      , getExpression      = optExpression opts
      , getOutputDir       = optOutputDir opts
      , getLanguageData    = langData
      , getLevel           = level
      , getLevelCode       = levelCode
      , getWorkingDir      = workingDir
      , getInputFileName   = inputFileName
      , getTempDirName     = ""
      }
  (_,_,errs) -> exitWithNativeError False wrongUsageError

--------------------------------------------------------------------------------
-- ...                                                                        --
--------------------------------------------------------------------------------

-- | Reads the level code string from the input file and returns it.
--
-- 'readLevelCode' fails with a native error message if the given input file
-- does not exist.
readLevelCode :: Bool -> FilePath -> IO String
readLevelCode v levelCodeFile = do
  when v $ putStr "Reading level code from file ... "
  when v $ hFlush stdout

  fileExists <- doesFileExist levelCodeFile
  when (not fileExists) $
    exitWithNativeError v $ couldNotFindLevelCodeFileError levelCodeFile

  levelCode <- readFile levelCodeFile
  when v $ putStrLn "done."
  return levelCode

-- | Reads the language data object from the language configuration file and
-- returns it.

-- 'readLanguageData' fails with native error messages if the configuration file
-- does not exist or if decoding fails.
readLanguageData :: Bool -> FilePath -> IO LanguageData
readLanguageData v configFile = do
  when v $ putStr "Reading language data from file ... "
  when v $ hFlush stdout

  fileExists <- doesFileExist configFile
  when (not fileExists) $
    exitWithNativeError v $ couldNotFindLanguageConfigFileError configFile

  langDataObj <- B.readFile configFile
  let mLangData = (decode langDataObj :: Maybe LanguageData)
  when (isNothing mLangData) $
    exitWithNativeError v $ couldNotDecodeLanguageConfigFileError configFile

  let langData = fromJust mLangData
  when v $ putStrLn "done."
  return langData

-- | Writes the given object code files (specified via pairs containing the file
-- base name and the source code) to an arbitrary directory and returns a pair
-- of lists @(written,failed)@ where @written@ contains all files that were
-- successfully written to the directory and @failed@ contains all files that do
-- already exist.
--
-- The file paths in @written@ and @failed@ are relative (absolute) if the given
-- directory path is relative (absolute).
--
-- It is assumed that the given directory exists. Thus 'writeObjectCodeToDir'
-- fails if the directory does not exist.
writeObjectCodeToDir
  :: FilePath
  -> [(String,String)]
  -> LanguageData
  -> IO ([FilePath],[FilePath])
writeObjectCodeToDir dir objectCode langData =
  foldrM (\(fileBaseName,fileContent) (written,failed) -> do
    let file = dir </> fileBaseName <.> objectCodeFileType langData
    fileExists <- doesFileExist file
    if fileExists
      then return (written,file:failed)
      else do writeFile file fileContent
              return (file:written,failed))
    ([],[]) objectCode

maybeWriteObjectCodeToOutputDir :: RunData -> [(String,String)] -> IO ()
maybeWriteObjectCodeToOutputDir runData objectCode = do
  let v          = getVerboseMode runData
  let mOutputDir = getOutputDir runData
  let langData   = getLanguageData runData

  when (isJust mOutputDir) $ liftIO $ do
    let outputDir = fromJust mOutputDir

    when v $ putStr "Writing object code to output directory ... "
    when v $ hFlush stdout

    fileExists <- doesFileExist outputDir
    if fileExists then do
      when v $ putStrLn "failed."
      putNativeWarnings v [couldNotCreateOutputDirectoryWarning outputDir]
    else do
      createDirectoryIfMissing True outputDir
      (written,failed) <- writeObjectCodeToDir outputDir objectCode langData
      if null failed then
        when v $ putStrLn "done."
      else
        if null written then do
          when v $ putStrLn "failed."
          putNativeWarnings v $ map couldNotWriteFileToOutputWarning failed
          -- NOTE maybe give the user the possibility to overwrite files here
        else do
          when v $ putStrLn "partially failed."
          putNativeWarnings v $ map couldNotWriteFileToOutputWarning failed
          -- NOTE maybe give the user the possibility to overwrite files here

writeObjectCodeToTempDir :: RunData -> [(String,String)] -> IO [FilePath]
writeObjectCodeToTempDir runData objectCode = do
  let v           = getVerboseMode runData
  let langData    = getLanguageData runData
  let tempDirName = getTempDirName runData

  when v $ putStr "Writing object code to temporary directory ... "
  when v $ hFlush stdout
  (files,_) <- writeObjectCodeToDir tempDirName objectCode langData
  when v $ putStrLn "done."
  return files

cleanUp :: Bool -> FilePath -> IO ()
cleanUp v tempDir = do
  when v $ putStr "Performing clean-up operations ... "
  when v $ hFlush stdout

  dirExists <- doesDirectoryExist tempDir
  when dirExists $ do
    removeDirectoryRecursive tempDir

  when v $ putStrLn "done."

--------------------------------------------------------------------------------
-- Running the executable                                                     --
--------------------------------------------------------------------------------

run :: RunData -> RunResult ExitCode
run runData = do
  let compileOnlyMode = getCompileOnlyMode runData
  let v               = getVerboseMode runData
  let workingDir      = getWorkingDir runData

  -- compile the level code
  (objCode,compData) <- compileLevelCodeWithWarnings runData
  -- maybe write object code files to output directory
  liftIO $ maybeWriteObjectCodeToOutputDir runData objCode

  if not compileOnlyMode then do
    -- write object code files to temporary directory
    liftIO $ setCurrentDirectory workingDir
    files <- liftIO $ writeObjectCodeToTempDir runData objCode
    -- execute object code
    executeObjectCode runData compData files
  else return ExitSuccess

--------------------------------------------------------------------------------
-- Level code compilation and object code execution                           --
--------------------------------------------------------------------------------

compileLevelCodeWithWarnings
  :: RunData
  -> RunResult ([(String,String)],CompilationData)
compileLevelCodeWithWarnings runData = do
  let v             = getVerboseMode runData
  let r             = getRawMode runData
  let d             = getDebugMode runData
  let mExpr         = getExpression runData
  let level         = getLevel runData
  let levelCode     = getLevelCode runData
  let inputFileName = getInputFileName runData

  when v $ liftIO $ putStr "Compiling level code ... "
  when v $ liftIO $ hFlush stdout
  ((res,compData),ws) <- runWithWarnings$compileLevelCode level levelCode mExpr d
  when v $ liftIO $ putStrLn "done."
  liftIO $ putWarnings r inputFileName v ws
  return (res,compData)

executeObjectCode
  :: RunData
  -> CompilationData
  -> [FilePath]
  -> RunResult ExitCode
executeObjectCode runData compData files = do
  let v           = getVerboseMode runData
  let langData    = getLanguageData runData

  when v $ liftIO $ putStr "Executing object code ... "
  when v $ liftIO $ hFlush stdout

  -- get execution commands
  let execCmdPatterns = executionCommandPatterns langData
  when (null execCmdPatterns) $
    liftIO $ exitWithNativeError v couldNotFindExecutionCommandPatternsError
  let mainFile = head files
  let execCmds = map (buildExecutionCommand mainFile) execCmdPatterns

  when v $ liftIO $ putStrLn ""
  when v $ liftIO $ putStrLn ""

  -- perform execution steps
  mapM_ (performExecutionStep execCmds) execCmds
  return ExitSuccess
  where
    buildExecutionCommand :: String -> String -> Command
    buildExecutionCommand mainFile execCmdPattern =
      let execCmdPatternParts = words execCmdPattern
          cmdPattern          = head execCmdPatternParts
          argPatterns         = tail execCmdPatternParts
          cmd =
            if cmdPattern == "./<filePath>" || cmdPattern == "<filePath>"
              then dropExtension mainFile
              else cmdPattern
          args =
            map (\argPattern ->
              if argPattern == "<filePath>"
                then mainFile
                else argPattern) argPatterns
       in (cmd,args)
    performExecutionStep :: [Command] -> Command -> RunResult ()
    performExecutionStep execCmds execCmd = do
      let execContext = (execCmd,execCmds,files)
      runInteractiveProcessWithOutputTranslation runData compData execContext

runInteractiveProcessWithOutputTranslation
  :: RunData
  -> CompilationData
  -> ExecutionContext
  -> RunResult ()
runInteractiveProcessWithOutputTranslation runData compData execContext = do
  let r                = getRawMode runData
  let inputFileName    = getInputFileName runData
  let ((cmd,args),_,_) = execContext

  -- spawn the interactive process
  let cmdFull = intercalate " " $ cmd:args++["2>&1"]
  (hin,hout,_,ph) <- liftIO $ runInteractiveCommand cmdFull
  --(hin,hout,herr,ph) <- liftIO $ runInteractiveProcess cmd args Nothing Nothing
  liftIO $ hSetBuffering hin  LineBuffering
  liftIO $ hSetBuffering hout NoBuffering
  -- liftIO $ hSetBuffering herr NoBuffering

  -- initialize failure and control variables
  failedWith <- liftIO $ newEmptyMVar
  done       <- liftIO $ newEmptyMVar

  -- input stream handler
  liftIO $ forkIO $ do
    contents <- getContents
    hPutStr hin contents
    `finally` putMVar done ()

  -- merged out/err stream handler
  liftIO $ forkIO $ do
    contents <- hGetContents hout
    let outFragments = translateOutput compData execContext contents
    putTranslatedOutput r inputFileName failedWith outFragments
    `finally` putMVar done ()

  -- wait for the process to finish
  liftIO $ takeMVar done
  liftIO $ waitForProcess ph

  -- throw error if execution failed
  success <- liftIO $ isEmptyMVar failedWith
  when (not success) $ do
    err <- liftIO $ takeMVar failedWith
    throwError [err]
  return ()

putTranslatedOutput
  :: Bool
  -> FilePath
  -> MVar RunError
  -> [OutputFragment]
  -> IO ()
putTranslatedOutput _ _ _          []                  = return ()
putTranslatedOutput _ _ failedWith [ErrorFragment err] = putMVar failedWith err
putTranslatedOutput r inputFileName failedWith (f:fs) = do
   case f of
     TextFragment text       -> putStr text
     WarningFragment warning -> putWarning r inputFileName warning
     ErrorFragment err       -> putError   r inputFileName err
   putTranslatedOutput r inputFileName failedWith fs

--------------------------------------------------------------------------------
-- Native warnings and errors                                                 --
--------------------------------------------------------------------------------

putNativeWarning :: String -> IO ()
putNativeWarning warningMsg = do
  hPutStrLn stderr $ newNativeWarningMessage warningMsg

putNativeWarnings :: Bool -> [String] -> IO ()
putNativeWarnings _     []          = return ()
putNativeWarnings addLn warningMsgs = do
  when addLn $ putStrLn ""
  mapM_ (\w -> putNativeWarning w >> putStrLn "") warningMsgs

putNativeError :: String -> IO ()
putNativeError errMsg = do
  hPutStrLn stderr $ newNativeErrorMessage errMsg

exitWithNativeError :: Bool -> String -> IO a
exitWithNativeError printFailed errMsg = do
  when printFailed $ putStrLn "failed."
  when printFailed $ putStrLn ""
  putNativeError errMsg
  putStrLn ""
  exitWith $ ExitFailure 1

--------------------------------------------------------------------------------
-- Compilation and execution warnings and errors                              --
--------------------------------------------------------------------------------

putWarning :: Bool -> FilePath -> RunWarning -> IO ()
putWarning r fileName warning = do
  let warningMessage = showWarning r fileName warning
  hPutStrLn stderr warningMessage

putWarnings :: Bool -> FilePath -> Bool -> [RunWarning] -> IO ()
putWarnings _ _        _     []       = return ()
putWarnings r fileName addLn warnings = do
  when addLn $ putStrLn ""
  mapM_ (\w -> putWarning r fileName w >> putStrLn "") warnings

putError :: Bool -> FilePath -> RunError -> IO ()
putError r fileName err = do
  let errorMessage = showError r fileName err
  hPutStrLn stderr errorMessage

putErrors :: Bool -> FilePath -> Bool -> [RunError] -> IO ()
putErrors _ _        _     []   = return ()
putErrors r fileName addLn errs = do
  when addLn $ putStrLn ""
  mapM_ (\e -> putError r fileName e >> putStrLn "") errs

issueHandler :: RunData -> [RunWarning] -> [RunError] -> IO ExitCode
issueHandler runData warnings [] = do
  let r             = getRawMode runData
  let inputFileName = getInputFileName runData
  putWarnings r inputFileName False warnings
  return ExitSuccess
issueHandler runData warnings errs = do
  let r             = getRawMode runData
  let v             = getVerboseMode runData
  let inputFileName = getInputFileName runData
  -- complete the verbosity output if necessary
  let source = errorSource $ head errs
  let v' = v && occuredWhileCompilation source
  when v' $ do
    putStrLn "failed."
  -- print warnings and errors
  putWarnings r inputFileName v' warnings
  putErrors   r inputFileName (v' && null warnings) errs
  -- get and return exit code
  let exitCode = errorToExitCode $ last errs
  return exitCode
  where
    occuredWhileCompilation :: Source -> Bool
    occuredWhileCompilation Lexer    = True
    occuredWhileCompilation Parser   = True
    occuredWhileCompilation Analyzer = True
    occuredWhileCompilation Printer  = True
    occuredWhileCompilation Compiler = True
    occuredWhileCompilation _        = False
    -- TODO map errors to different exit codes
    errorToExitCode :: RunError -> ExitCode
    errorToExitCode _ = ExitFailure 1

--------------------------------------------------------------------------------
