--------------------------------------------------------------------------------
-- ...
--------------------------------------------------------------------------------

module Executable.Messages (
  newTypedMessage,

  showWarning,
  showError,

  newNativeWarningMessage,
  couldNotCreateOutputDirectoryWarning,
  couldNotWriteFileToOutputWarning,

  newNativeErrorMessage,
  wrongUsageError,
  levelNotValidError,
  couldNotFindLevelCodeFileError,
  couldNotFindLanguageConfigFileError,
  couldNotDecodeLanguageConfigFileError,
  couldNotFindExecutionCommandPatternsError,

  usagePattern,
  helpHeader
) where

import Data.Char       (toLower,toUpper)
import Data.List.Utils (replace)
import System.FilePath (takeFileName)

import Executable.Types

--------------------------------------------------------------------------------
-- Typed messages                                                             --
--------------------------------------------------------------------------------

newTypedMessage :: String -> [(String,String)] -> String -> String -> String
newTypedMessage msgType attrs head body =
  "<message type=\""++msgType++"\""++attrsStr++">\n\
  \<head>\n\
  \"++head++"\n\
  \</head>\n\
  \<body>\n\
  \"++body++"\n\
  \</body>\n\
  \</message>"
  where
    attrsStr :: String
    attrsStr = concatMap (\(n,v) -> " "++n++"=\""++v++"\"") attrs

--------------------------------------------------------------------------------
-- Warning and error messages                                                 --
--------------------------------------------------------------------------------

showWarning :: Bool -> FilePath -> RunWarning -> String
showWarning r fileName warning =
  let source = warningSource warning
      (mPos,cont) = case warningData warning of
        Left       cont  -> (Nothing ,cont)
        Right (pos,cont) -> (Just pos,cont)
  in  showIssue r fileName "warning" source mPos cont

showError :: Bool -> FilePath -> RunError -> String
showError r fileName err =
  let source = errorSource err
      (mPos,cont) = case errorData err of
        Left       cont  -> (Nothing ,cont)
        Right (pos,cont) -> (Just pos,cont)
  in  showIssue r fileName "error" source mPos cont

showIssue :: Bool -> FilePath -> String -> Source -> Maybe Pos -> String -> String
showIssue r fileName issueType source mPos cont = case r of
    True  -> head++"\n"++body
    False -> newTypedMessage issueType attrs head body
  where
    attrs :: [(String,String)]
    attrs = ("data-source",map toLower $ show source) : case mPos of
      Nothing          -> []
      (Just (_,-1,-1)) -> []
      (Just (_,r ,-1)) -> [("data-row",show r)]
      (Just (_,r ,c )) -> [("data-row",show r),("data-col",show c)]
    head :: String
    head = fileName++":"++posStr++issueStr++":"
    body :: String
    body = indent 2 cont
    posStr :: String
    posStr = (concatMap ((++":").snd) $ tail attrs)++" "
    issueStr :: String
    issueStr = case source of
      Other -> toUpper (Prelude.head issueType) : tail issueType
      _     -> show source++" "++issueType

--------------------------------------------------------------------------------
-- Native messages and message parts                                          --
--------------------------------------------------------------------------------

newNativeWarningMessage :: String -> String
newNativeWarningMessage cont =
  "run: Warning:\n\
  \"++indent 2 cont

couldNotCreateOutputDirectoryWarning :: FilePath -> String
couldNotCreateOutputDirectoryWarning _ =
  "Could not create output directory folder\n\
  \A file with the same name does already exist"

couldNotWriteFileToOutputWarning :: FilePath -> String
couldNotWriteFileToOutputWarning file =
  "Could not write object code file \""++takeFileName file++"\"\n\
  \to the specified output directory\n\
  \A file with the same name does already exist"

--------------------------------------------------------------------------------

newNativeErrorMessage :: String -> String
newNativeErrorMessage cont =
  "run: Error:\n\
  \"++indent 2 cont

wrongUsageError :: String
wrongUsageError =
  "Wrong usage\n\
  \Usage:\n\
  \\n\
  \  "++usagePattern++"\n\
  \\n\
  \For more information, try the \"--help\" option."

levelNotValidError :: String -> String
levelNotValidError _ =
  "Level not valid (must be a natural number)"

couldNotFindLevelCodeFileError :: FilePath -> String
couldNotFindLevelCodeFileError _ =
  "Could not find level code file"

couldNotFindLanguageConfigFileError :: FilePath -> String
couldNotFindLanguageConfigFileError _ =
  "Could not find language configuration file"

couldNotDecodeLanguageConfigFileError :: FilePath -> String
couldNotDecodeLanguageConfigFileError _ =
  "Could not decode language configuration file\n\
  \\n\
  \In most cases decoding fails due to a JSON syntax\n\
  \error - try using JSONLint for syntax validation:\n\
  \\n\
  \  http://jsonlint.com"

couldNotFindExecutionCommandPatternsError :: String
couldNotFindExecutionCommandPatternsError =
  "Could not find execution command patterns\n\
  \\n\
  \Try checking if the \"executionCommandPatterns\"\n\
  \array is empty in the language configuration file"

--------------------------------------------------------------------------------

usagePattern :: String
usagePattern =
  "run [<options>] [<level> <levelCodeFilePath>]"

helpHeader :: String
helpHeader =
  "Usage:\n\
  \\n\
  \  "++usagePattern++"\n\
  \\n\
  \Options:\n"

--------------------------------------------------------------------------------
-- Helpers                                                                    --
--------------------------------------------------------------------------------

indent :: Int -> String -> String
indent n str = whitespaces ++ replace "\n" ("\n"++whitespaces) str
  where whitespaces = take n $ repeat ' '

--------------------------------------------------------------------------------
