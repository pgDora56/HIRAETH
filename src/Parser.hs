module Parser where

import Lexer

type Priority = Int
type Stack = [ExprOp]

parser :: [Token] -> [Token]
parser token = sort token []

sort :: [Token] -> Stack -> [Token]
sort [] [] = []
sort [] (s:stack) = (TokenE s):(sort [] stack)
sort ((TokenN n):tokens) stack = (TokenN n):(sort tokens stack)
sort ((TokenE BracOpen):tokens) stack = sort tokens (BracOpen:stack)
sort ((TokenE BracEnd):tokens) stack = bracket tokens stack
sort (TokenE e:tokens) [] = sort tokens (e:[])
sort (TokenE e:tokens) (s:stack) = 
    if checkPriority e s
        then sort tokens (e:s:stack)
        else (TokenE s):(sort ((TokenE e):tokens) stack)

bracket :: [Token] -> Stack -> [Token]
bracket tokens (BracOpen:stack) = sort tokens stack
bracket tokens (s:stack) = (TokenE s):(bracket tokens stack)

checkPriority :: ExprOp -> ExprOp -> Bool
checkPriority ex1 ex2 = (priority ex1) > (priority ex2)

priority :: ExprOp -> Priority
priority Plus = 1
priority Minus = 1
priority Mult = 2
priority BracOpen = 0
priority BracEnd = 0


