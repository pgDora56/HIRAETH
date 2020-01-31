module Parser where

import Data.Char (ord)
import Lexer

parser :: Expr -> Int
parser (ENumber num) = numParser num
parser (Expr exp1 Plus exp2) = (parser exp1) + (parser exp2)
parser (Expr exp1 Minus exp2) = (parser exp1) - (parser exp2)
parser (Expr exp1 Mult exp2) = (parser exp1) * (parser exp2)
-- parser (Expr exp1 Div exp2) = (parser exp1) % (parser exp2)


numParser :: Number -> Int
numParser NoNumber = 0
numParser (NDigit dig) = digParser dig
numParser (Number num dig) = 10 * numParser num + digParser dig


digParser :: Digit -> Int
digParser (Digit d) = ord d - ord '0'

parserTest :: Expr -> IO()
parserTest (Expr exp1 op exp2) = print exp1
