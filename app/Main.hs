module Main where

import Lexer

main :: IO ()
main = print $ lexer "10+20-30"
