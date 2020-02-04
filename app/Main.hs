module Main where

import Lexer (lexer)
import Parser (parser)
import Compiler (compile)

main :: IO ()
main = compile $ parser $ lexer "print(12+23);"
-- main = do
--     line <- getLine
--     if line == ""
--         then putStrLn "Exit"
--         else do 
--             print $ parser $ lexer line
--             main
