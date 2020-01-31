module Lexer where

data Expr = Expr Expr ExprOp Expr | ENumber Number deriving (Show)
data Number = NoNumber | NDigit Digit | Number Number Digit deriving (Show)
data Digit = Digit Char deriving (Show)
data ExprOp = Plus | Minus | Mult deriving (Show)

lexer :: String -> Expr
lexer = lexer' NoNumber

lexer' :: Number -> String -> Expr
lexer' num [] = ENumber num
lexer' NoNumber prg
    | p == '1' ||  p == '2' || p == '3' || p == '4' || p == '5' || p == '6' || p == '7' || p == '8' || p == '9' || p == '0' 
        = lexer' (NDigit $ Digit p) (tail prg)
    | p == '+' || p == '-' || p == '*' || p == '/'
        = ENumber NoNumber
    | otherwise = lexer $ tail prg
    where p = prg !! 0
lexer' num ('+':prg) = Expr (ENumber num) Plus (lexer' NoNumber prg)
lexer' num ('-':prg) = Expr (ENumber num) Minus (lexer' NoNumber prg)
lexer' num ('*':prg) = Expr (ENumber num) Mult (lexer' NoNumber prg)
lexer' num prg
    | p == '1' ||  p == '2' || p == '3' || p == '4' || p == '5' || p == '6' || p == '7' || p == '8' || p == '9' || p == '0' 
        = lexer' (Number num $ Digit p) (tail prg)
    | p == '\n'  = ENumber num
    | otherwise = lexer' num (tail prg)
    where p = prg !! 0

{-
type Symbol = Char
type Identifier = String
type Token = Symbol | Identifier

lexer :: String -> [Token]
lexer prg
    | p == '(' || p == ')' || p == ';' =  (p:(lexer $ tail prg))
    | p == '\n' || p == ' ' || p == '\t' = lexer $ tail prg
    | otherwise = subLexer "" prg
    where p = prg !! 0

subLexer :: String -> String -> [Token]
subLexer text prg
-}

{-
-- Program Example
--

100+10 -> 110


print("Hello, World");
var x;
var y;
x = 1;
y = 2;

 - BNF Define
 -

<Expr> := <Term> | <Term> <ExprOp> <Expr>
<Term> := <Number> | <Number> <Number>
<Number> := ("1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"0")+
<ExprOp> := "+" | "-"
<TermOp> := "*" | "/"

-}
