{
{-# OPTIONS_GHC -w #-}
module Lang.Gen.Lexer where

import qualified Lang.Gen.AST as AST (Pos)
}

%wrapper "monad"

@number              = [0-9]+

tokens :-

<0>"+"                                  {lift (\s -> TokenM877102771 s)}
<0>"-"                                  {lift (\s -> TokenM1906462069 s)}
<0>"*"                                  {lift (\s -> Token1785060526 s)}
<0>"/"                                  {lift (\s -> Token1359145929 s)}
<0>"("                                  {lift (\s -> TokenM1480547472 s)}
<0>")"                                  {lift (\s -> Token152256527 s)}
<0>@number                              {lift (\s -> TokenM585301981 s (read s ))}
<0>$white+                              { skip }

{
data Token = 
     TokenM877102771 String 
  |  TokenM1906462069 String 
  |  Token1785060526 String 
  |  Token1359145929 String 
  |  TokenM1480547472 String 
  |  Token152256527 String 
  |  TokenM585301981 String Int
  |  Token1967005722 String 
  |  EOF
	deriving (Show, Eq)
	
alexEOF = return (AlexPn 0 1 1, EOF)

scan :: String -> LexerResult
scan str = case (scan' str) of
		Right list  -> LexSuccess list
		Left string -> LexFail (getPosFromString string) string
		
scan' str = runAlex str $ do
               let loop = do (pos, tok) <- alexMonadScan
                             if tok == EOF
                               then return [(pos,tok)]
                               else do toks <- loop
                                       return $ (pos, tok) : toks
               loop	
               
getPosFromString :: String -> AST.Pos
getPosFromString str = (0, line, column)
						where
						[headElem, tailElem] = wordsWhen (==',') str
						line = (read $ drop 22 headElem)::Int
						column = (read $ drop 8 tailElem)::Int
			 
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

data LexerResult = LexSuccess [(AlexPosn, Token)] | LexFail AST.Pos String
	deriving Show	

lift :: (String -> Token) -> AlexInput -> Int -> Alex (AlexPosn, Token)
lift f (pos,_,_,inputString) len = return $ (pos, f $ take len inputString)
	
getPos :: (AlexPosn, Token) -> AST.Pos
getPos (AlexPn a l c,_) = (a,l,c)

getToken :: (AlexPosn, Token) -> Token
getToken (_,t) = t

getnumberValue (TokenM585301981 _ x) = x

tokenToString(TokenM877102771 x) = x
tokenToString(TokenM1906462069 x) = x
tokenToString(Token1785060526 x) = x
tokenToString(Token1359145929 x) = x
tokenToString(TokenM1480547472 x) = x
tokenToString(Token152256527 x) = x
tokenToString(TokenM585301981 x _) = x
tokenToString(Token1967005722 x) = x
tokenToString EOF  = "eof"
}
