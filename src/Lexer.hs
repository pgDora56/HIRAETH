module Lexer where


type Symbol = Char
type Identifier = String
type Token = Symbol | Identifier

lexer :: String -> [Token]
lexer (p:prg)
    | p == '(' || p == ')' || p == ';' =  (p:(lexer prg))
    | p == '\n' || p == ' ' = lexer prg
    | otherwise = subLexer p prg

subLexer :: String -> String -> [Token]

{-
-- Program Example
--
-- print("Hello, World");
 - var hello;
 -}
