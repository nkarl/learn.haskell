-- Haskell, Practicing the Basics
-- Name: Charles Norden

aa = [1, 2, 3]

-- 1. Copy a list and return a new list
-- aa = [1,2,3] --> bb = [1,2,3]
-- cpList :: [a] -> [a]
-- cpList iL = cpListUnop iL
--   where
--     cpListUnop :: [a] -> [a]
--     cpListUnop [] = [] -- base case, when passed list is empty; returns an empty list
--     cpListUnop (x : xs) = x : (cpListUnop xs) -- inductive case, when passed list is not empty; treat the next recursive call as a list, append x to that

-- Copy a list in reverse; unoptimized
rev'cpListUnop :: [a] -> [a]
rev'cpListUnop iL =
  let cpListUnop :: [a] -> [a]
      cpListUnop [] = []
      cpListUnop (x : xs) = x : (cpListUnop xs)
   in reverse (cpListUnop iL)

-- CopyList, tail optimized
-- [1,2,3] --> [2,3] and 1:[] --> [3] and 2:[1] --> [] and 3:[2,1] --> [3,2,1]
tailcpList :: [a] -> [a]
tailcpList iL = cpListTail iL []
  where
    cpListTail :: [a] -> [a] -> [a]
    cpListTail [] buff = buff
    cpListTail (x : xs) buff = cpListTail xs (x : buff)
