--------------------------------------------------------------------------------
-- ...
--------------------------------------------------------------------------------

module Lang.Interface (
  CompilationData,
  compileLevelCode,
  translateOutput
) where

import Control.Monad.Error (liftIO,throwError,when)

import Executable.Types

import Lang.Gen.Parser0 as Parser0
import Lang.Gen.Parser1 as Parser1
import Lang.Gen.PrettyPrinter as PrettyPrinter
import Lang.ParserTypes as ParserTypes
import Lang.Gen.AST as AST
import Lang.PPDoc as PPDoc

--------------------------------------------------------------------------------
-- Compiling level code                                                       --
--------------------------------------------------------------------------------

type CompilationData = ()
-- or data CompilationData = ...

-- Int: Level
-- String: Actual code to compile
-- Maybe String: Expression
-- Bool: True if output should include debug information
compileLevelCode
  :: Int
  -> String
  -> Maybe String
  -> Bool
  -> RunResult ([(String,String)],CompilationData)
compileLevelCode level levelCode mExpr includeDebug =
  case level of
    0 -> return ([("Main", "puts(" ++ parseWrapper levelCode Parser0.parse ++ ");")],())
    1 -> return ([("Main", "puts(" ++ parseWrapper levelCode Parser1.parse ++ ");")],())

parseWrapper :: String -> (String -> ParserTypes.ParseResult AST.AST) -> String
parseWrapper code fun = case fun code of
  Ok ast -> PPDoc.getPPString $ PrettyPrinter.ppAST ast
  ParseError _ strings  -> error $ concat ("Parser error: " : strings)
  LexerError _ errorMsg -> error $ "Lexer error: " ++ errorMsg

--------------------------------------------------------------------------------
-- Translating execution output                                               --
--------------------------------------------------------------------------------

translateOutput
  :: CompilationData
  -> ExecutionContext
  -> String
  -> [OutputFragment]
translateOutput _ _ ""  = []
translateOutput _ _ out =
  -- to be implemented
  [TextFragment out]

--------------------------------------------------------------------------------
