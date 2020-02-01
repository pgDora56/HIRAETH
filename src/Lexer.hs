module Lexer where

import Data.Char (ord)

data Token = TokenN Number | TokenE ExprOp deriving (Show)
data Number = Number Int deriving (Show)
data ExprOp = Plus | Minus | Mult | BracOpen | BracEnd deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('+':prg) = (TokenE Plus):(lexer prg)
lexer ('-':prg) = (TokenE Minus):(lexer prg)
lexer ('*':prg) = (TokenE Mult):(lexer prg)
lexer ('(':prg) = (TokenE BracOpen):(lexer prg)
lexer (')':prg) = (TokenE BracEnd):(lexer prg)
lexer (p:prg) = 
    if p == '1' || p == '2' || p == '3' || p == '4' || p == '5' || p == '6' || p == '7' || p == '8' || p == '9' || p == '0' 
        then nlexer (cton p) prg 
        else lexer prg

nlexer :: Int -> String -> [Token]
nlexer n [] = [TokenN $ Number n] 
nlexer n prg
    | p == '1' || p == '2' || p == '3' || p == '4' || p == '5' || p == '6' || p == '7' || p == '8' || p == '9' || p == '0' 
        = nlexer (n * 10 + (cton p)) (tail prg)
    | otherwise = (TokenN $ Number n):(lexer prg)
    where p = prg !! 0

cton :: Char -> Int
cton c = ord c - ord '0'

