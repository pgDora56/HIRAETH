module Lexer where

import Data.Char (ord, isDigit, isAlphaNum)

data Token = TokenN Number | TokenE ExprOp | TokenS Symbol| TokenI Ident deriving (Show)
data Number = Number Int deriving (Show) -- 数字
data Ident = Ident String deriving(Show) -- 識別子
data ExprOp = Plus | Minus | Mult | Brac deriving (Show) -- 加減乗除
data Symbol = Assign | SemiCoron | BracOpen | BracEnd deriving (Show) -- 記号

lexer :: String -> [Token]
lexer [] = []
lexer ('+':prg) = (TokenE Plus):(lexer prg)
lexer ('-':prg) = (TokenE Minus):(lexer prg)
lexer ('*':prg) = (TokenE Mult):(lexer prg)
lexer ('(':prg) = (TokenS BracOpen):(lexer prg)
lexer (')':prg) = (TokenS BracEnd):(lexer prg)
lexer ('<':('-':prg)) = (TokenS Assign):(lexer prg)
lexer (';':prg) = (TokenS SemiCoron):(lexer prg)
lexer (p:prg)  
    | isDigit p = nlexer (cton p) prg 
    | isAlphaNum p = clexer [p] prg
    | otherwise = lexer prg

nlexer :: Int -> String -> [Token]
nlexer n [] = [TokenN $ Number n] 
nlexer n prg
    | isDigit p = nlexer (n * 10 + (cton p)) (tail prg)
    | otherwise = (TokenN $ Number n):(lexer prg)
    where p = prg !! 0

clexer :: String -> String -> [Token]
clexer ident [] = [TokenI $ Ident ident]
clexer ident prg
    | isAlphaNum p = clexer (ident++[p]) (tail prg)
    | otherwise = (TokenI $ Ident ident):(lexer prg)
    where p = prg !! 0

cton :: Char -> Int
cton c = ord c - ord '0'

