{
module Lang.Gen.Parser1 where

import qualified Lang.Gen.AST as AST
import qualified Lang.Gen.Lexer as L
import Lang.ParserTypes
}

%name parseTokens
%monad { ParseResult }
%error { parseError }
%tokentype { (L.AlexPosn, L.Token) }

%token
    EOF { (_,L.EOF) }
	"+"	 { (_, L.TokenM877102771 _) }
	"-"	 { (_, L.TokenM1906462069 _) }
	"*"	 { (_, L.Token1785060526 _) }
	"/"	 { (_, L.Token1359145929 _) }
	"("	 { (_, L.TokenM1480547472 _) }
	")"	 { (_, L.Token152256527 _) }
	number	 { (_, L.TokenM585301981 _ _) }
	whitespace	 { (_, L.Token1967005722 _) }

%left "+" "-"
%left "*" "/"

%%

s : OuterExpression EOF { $1 }

OuterExpression : Exp {PosV (pos $1) (AST.OuterExpression (pos $1) (val $1))}

Exp : Exp "+" Exp {PosV (pos $1) (AST.Addition (pos $1) (val $1) (val $3))}
    | Group       {PosV (pos $1) (AST.Subtraction (pos $1) (Just (val $1)))}
    | Exp "*" Exp {PosV (pos $1) (AST.Multiplication (pos $1) (val $1) (val $3))}
    | Group0      {PosV (pos $1) (AST.Division (pos $1) (Just (val $1)))}
    | number      {PosV (L.getPos $1) (AST.Number (L.getPos $1) (L.getPos $1, (L.getnumberValue $ L.getToken $1)))}
    | "(" Exp ")" {PosV (L.getPos $1) (AST.Parentheses (L.getPos $1) (val $2))}

Group : Exp "-" Exp {PosV (pos $1) (AST.Group (pos $1) (val $1) (val $3))}

Group0 : Exp "/" Exp {PosV (pos $1) (AST.Group0 (pos $1) (val $1) (val $3))}
	
{		
parse :: String -> ParseResult AST.AST
parse str = lexwrap str >>= parseTokens >>= return . val
}
