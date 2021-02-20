-- CptS 355 - Spring 2021 -- Homework1 - Haskell
-- Name: Charles Norden
-- Collaborators: N/A

module HW1 where

getUniqueRight :: Eq a => [a] -> [a]
getUniqueRight iL = reverse (helper (reverse iL) [])
  where
    helper [] _ = []
    helper (x : xs) buf
      | (x `elem` buf) = helper xs buf
      | otherwise = x : helper xs (x : buf)

-- Q1(b) getUniqueLeft
getUniqueLeft :: Eq a => [a] -> [a]
getUniqueLeft iL = helper iL []
  where
    helper [] buf = reverse buf
    helper (x : xs) buf
      | (x `elem` buf) = helper xs buf
      | otherwise = helper xs (x : buf)


-- Q2(a) cansInLog
cansInLog :: Num p => [(a, p)] -> p
cansInLog [] = 0
cansInLog ((x, y) : xs) = cansInLog xs + y

-- Q2(b) numCans
numCans :: (Num p, Eq t) => [((a1, t), [(a2, p)])] -> t -> p
numCans [] _ = 0
numCans (x : xs) year = getCansHelper x xs year
  where
    getCansHelper ((t, v), ts) [] year
      | (v == year) = cansInLog ts
      | otherwise = 0
    getCansHelper ((x, y), list) (t : xs) year
      | (y == year) = getCansHelper t xs year + cansInLog list
      | otherwise = getCansHelper t xs year

getMonths :: (Ord t1, Eq t2) => [(a, [(t2, t1)])] -> t1 -> t2 -> [a]
getMonths iL n f = helper iL f n []
  where -- Helper1: to scope out functionalities
    helper [] _ _ buf = reverse buf                                                     -- the base case, when the list is finally empty
    helper (monthList : log) f n buf = helper log f n (helper2 monthList f n buf)       -- the recursive case
      where -- Helper2: to check if the flavor exists and if value of cans qualify
        helper2 (m, []) f n buf = buf                                                   -- the base case
        helper2 (m, (x, y) : xs) f n buf                                                -- the recursive case
          | (x == f) && (y > n) = m : buf
          | otherwise = helper2 (m, xs) f n buf


-- Q3 deepCount
deepCount :: (Eq t1, Num t2) => t1 -> [[t1]] -> t2
deepCount v = helper v 0
  where
    helper v n [] = n
    helper v n (x : xs)
      | (v `elem` x) = (helper2 v n x) + (helper v n xs)
      | otherwise = helper v n xs
      where
        helper2 v n [] = n
        helper2 v n (x : xs)
          | (x == v) = helper2 v (n + 1) xs
          | otherwise = helper2 v n xs


-- Q4 clusterConsecutive
clusterConsecutive :: (Num a, Ord a) => [a] -> [[a]]
clusterConsecutive [] = []
clusterConsecutive (x : xs) = helper x xs [] []
  where
    helper x [] buf b = reverse (reverse (x : buf) : b)
    helper x (y : xs) buf b
      | (x + 1 == y) = helper y xs (x : buf) b
      | (x + 1 < y) || (x > y) = helper y xs [] (reverse (x : buf) : b)
      | otherwise = helper y xs buf b
