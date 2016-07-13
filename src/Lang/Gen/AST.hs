module Lang.Gen.AST where

type Pos = (Int,Int,Int)

type AST = OuterExpression

data OuterExpression = OuterExpression Pos Exp
    deriving Show

data Exp = Addition Pos Exp  Exp
    | Subtraction Pos (Maybe Group)
    | Multiplication Pos Exp  Exp
    | Division Pos (Maybe Group0)
    | Number Pos (Pos, Int)
    | Parentheses Pos  Exp 
    deriving Show

data Group = Group Pos Exp  Exp
    deriving Show

data Group0 = Group0 Pos Exp  Exp
    deriving Show

