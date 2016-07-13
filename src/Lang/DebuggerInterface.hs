module Lang.DebuggerInterface where

{-
The imports section should contain all the instructions needed to load
libraries required by your implementation of the debug functions.
For instance, you might want to load a library that allows your language
to work with TCP sockets etc. You must place the imports at the proper place within the PrettyPrinter yourself, as this is language specific.
-}
imports :: String
imports = ""

{-
The requiredHelperMethods section contains all the methods you define to 
implement the following functions, such as sending a message to the debugger via
a TCP channel. You must place the imports at the proper place within the PrettyPrinter yourself, as this is language specific.
-}
requiredHelperMethods :: String
requiredHelperMethods = ""

-- ##############################################
-- The following functions should all return Strings representing method implementations in your language
-- ##############################################

{-
The updatePosition method is called whenever a (Int, Int, Int) object is found in the AST, 
i.e. before any source code instruction that has a position attached to it.

Your implementation has to take this (Int, Int, Int) object, extract the line and column number,
package it into a UPDATEPOSITION message and send it to the debugger. Then your implementation
has to block until it receives a STEP message. Your implementation must not continue under any
circumstances without receiving a STEP message.
-}
updatePosition :: (Int, Int, Int) -> String
updatePosition pos = ""

{-
The updateTable method is called whenever a variable is changed. 

-- variableName: Name of the updated variable
-- variableValue: Value of the updated variable
-- variableAddress: Address of the updated variable
-- options: List of Strings. Strings must be Key/Value pairs, with =*= serving as the assign symbol, i.e. key=*=value

No acknowledgement from the debugger is required, i.e. your implementation should not block waiting for a reply.
-}
updateTable :: String -> String -> String -> [String] -> String
updateTable variableName variableValue variableAddress options = ""

{-
The pushOntoCallStack method is called whenever a method call is encountered.

-- methodName: Name of the called method
-- params: A list of Strings containing the parameters
-- options: List of Strings. Strings must be Key/Value pairs, with =*= serving as the assign symbol, i.e. key=*=value

 
No acknowledgement from the debugger is required, i.e. your implementation should not block waiting for a reply.
-}
pushOntoCallStack :: String -> [String] -> [String] -> String
pushOntoCallStack methodName params options =  ""

{-
The popFromCallStack method is called whenever a method call ends.

Your implementation has to send a POPFROMCALLSTACK message to the debugger. No acknowledgement from the debugger is
required, i.e. your implementation should not block waiting for a reply.
-}
popFromCallStack :: String
popFromCallStack = ""
