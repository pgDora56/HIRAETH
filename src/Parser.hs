module Parser where

import Lexer

type Priority = Int
type Stack = [ExprOp]

data Program = PrgNone | PrgP PrintExpr | PrgA AssignExpr | Prg Program Program deriving (Show) 
data PrintExpr = Print Expr deriving (Show)
data AssignExpr = AssignExpr Ident Expr deriving (Show)
data Expr = ExpNone | ExpI Ident | ExpN Number | ExpO ExprOp | Expr Expr Expr deriving (Show) -- 式

parser :: [Token] -> Program
parser [] = PrgNone
parser ((TokenI (Ident "print")):tokens)  
    | (length $ snd sep) == 0 = PrgP $ Print $ sort (fst sep) []
    | otherwise             = Prg (PrgP $ Print $ sort (fst sep) []) $ parser $ snd sep 
    where sep = separateExp tokens []
parser ((TokenI ident):((TokenS Assign):tokens)) 
    | (length $ snd sep) == 0 = PrgA $ AssignExpr ident $ sort (fst sep) []
    | otherwise             = Prg (PrgA $ AssignExpr ident $ sort (fst sep) []) $ parser $ snd sep
    where sep = separateExp tokens []


separateExp :: [Token] -> [Token] -> ([Token], [Token])
separateExp ((TokenS SemiCoron):tokens) exp = (exp, tokens)
separateExp (t:tokens) exp = separateExp tokens (exp++[t])

{-
expParser :: [Token] -> [Token]
expParser token = sort token []
-}

sort :: [Token] -> Stack -> Expr
sort [] [] = ExpNone
sort [] (s:stack) = Expr (ExpO s) $ sort [] stack
sort ((TokenN n):tokens) stack = Expr (ExpN n) $ sort tokens stack
sort ((TokenI n):tokens) stack = Expr (ExpI n) $ sort tokens stack
sort ((TokenS BracOpen):tokens) stack = sort tokens (Brac:stack)
sort ((TokenS BracEnd):tokens) stack = bracket tokens stack
sort (TokenE e:tokens) [] = sort tokens (e:[])
sort (TokenE e:tokens) (s:stack) = 
    if checkPriority e s
        then sort tokens (e:s:stack)
        else Expr (ExpO s) $ sort ((TokenE e):tokens) stack
sort _ _ = ExpNone

bracket :: [Token] -> Stack -> Expr
bracket tokens (Brac:stack) = sort tokens stack
bracket tokens (s:stack) = Expr (ExpO s) $ bracket tokens stack

checkPriority :: ExprOp -> ExprOp -> Bool
checkPriority ex1 ex2 = (priority ex1) > (priority ex2)

priority :: ExprOp -> Priority
priority Plus = 1
priority Minus = 1
priority Mult = 2
priority Brac= 0


