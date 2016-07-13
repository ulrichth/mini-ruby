module Lang.ParserTypes where

import qualified Lang.Gen.AST as AST
import qualified Lang.Gen.Lexer as L
import Control.Monad
import Control.Applicative

data ParseResult a = Ok a
                   | ParseError AST.Pos UnmatchedTerminals
                   | LexerError AST.Pos ErrMsg
    deriving Show

type UnmatchedTerminals = [String]
type ErrMsg = String

instance Monad ParseResult where
    return x = Ok x
    m >>= f = case m of
        Ok x -> f x
        ParseError p ts -> ParseError p ts
        LexerError p emsg -> LexerError p emsg

instance Applicative ParseResult where
    pure  = return
    (<*>) = ap
  
instance Functor ParseResult where
    fmap = prMap
  
lexwrap :: String -> ParseResult [(L.AlexPosn,L.Token)]
lexwrap str = case (L.scan str) of
    L.LexSuccess list -> Ok list
    L.LexFail p err -> LexerError p err

prMap :: (a -> b) -> ParseResult a -> ParseResult b
prMap f (Ok x) = Ok (f x)
prMap f (ParseError pos strings) = ParseError pos strings

failPR :: [(L.AlexPosn,L.Token)] -> ParseResult e
failPR []      = ParseError (0,0,1) []
failPR (t:tks) = ParseError (L.getPos t) (map (L.tokenToString . L.getToken) (t:tks))

parseError x = failPR x

data PosAndValue a = PosV { pos :: AST.Pos, val :: a }