module Main where

import Lib

rule ' ' ' ' ' ' = ' '
rule ' ' ' ' 'X' = 'X'
rule ' ' 'X' ' ' = ' '
rule ' ' 'X' 'X' = 'X'
rule 'X' ' ' ' ' = 'X'
rule 'X' ' ' 'X' = ' '
rule 'X' 'X' ' ' = 'X'
rule 'X' 'X' 'X' = ' '

start n = replicate n ' ' ++ "X" ++ replicate n ' '

next (a : b : c : rest) = rule a b c : next (b : c : rest)
next _ = " "

rows n = take n $ iterate (\x -> ' ' : next x) $ start n

main :: IO ()
main = mapM_ putStrLn $ rows 16
    
