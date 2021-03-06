module HigherOrder_basic
    where

import Data.Char


-- EXAMPLES OF MAPPING FUNCTIONS
-------------------------------------------------------------------------------------------------------------
-- ORIGINAL FUNCTIONS:
--
-- copyList
copyList :: [a] -> [a]
copyList []     = []
copyList (x:xs) = x : (copyList xs)

-- allSquares
allSquares :: Num a => [a] -> [a]
allSquares []       = []
allSquares (x:xs)   = x*x : (allSquares xs)

-- strToUpper
strToUpper :: String -> String
strToUpper []       = []
strToUpper (c:xs)   = (toUpper c) : (strToUpper xs)

-- implement our own mapper
mapper :: (t -> a) -> [t] -> [a]
mapper op []        = []
mapper op (x:xs)    = (op x) : (mapper op xs)



-- list for testing with mapping
aa=[1,2,3,4,5,6,7,8]
-------------------------------------------------------------------------------
-- REWRITTEN TO SUPPORT MAPPING:
--
allSquare2 xs   = mapper square xs
                   where square x = x*x

allSquare3 xs   = mapper (\x -> x*x) xs


-- multiply function
multiplyBy n x = n*x

multiplyAll xs n    = mapper (multiplyBy n) xs

multiplyAll2 xs n   = mapper (\x -> x*n) xs


