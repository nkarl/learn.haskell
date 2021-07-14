module Main where

import Lib
import System.Info
import System.Environment
import Data.List

{-|
 - This file contains popular examples of Haskell libraries and how to use them.
 -}

main :: IO ()
--main = someFunc

-- data structures
list   = [1, 2, 3, 4, 5]

tuple  = (1, 2)
tuple3 = (1, 2, 3)

first  (a, _, _)  = a
second (_, b, _)  = b
third  (_, _, c)  = c


main = do
    {--
    print os
    print arch
    print compilerName
    print compilerVersion
   
    getArgs >>= print
    getProgName >>= print
    getEnvironment >>= print
    print list
    --}

    print $ head list
    print $ tail list
    print $ last list
    print $ init list

    print $ list !! 3
    print $ elem 3 list
    
    print $ length list
    print $ null list
    print $ reverse list

    print $ take 2 list
    print $ drop 2 list

    print tuple
    print $ fst tuple
    print $ snd tuple

    print tuple3
    print $ first tuple3
    print $ second tuple3
    print $ third tuple3

    print $ intersperse '.' "Erik"
    --print $ intercalate ' ' ["abc", "efg", "x"]
    print $ unwords ["abc", "efg", "x"]

    putStrLn "hello world"
