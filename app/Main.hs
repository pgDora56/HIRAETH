module Main where

import Lexer
import Parser (parser)

main :: IO ()
main = do
    line <- getLine
    if line == ""
        then putStrLn "Exit"
        else do 
            print $ parser $ lexer line
            main

