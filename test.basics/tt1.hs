-- 4 problems in total
-- 1. getUniqueRight and getUniqueLeft 20%
-- 2. cansInLog and numCangs and getMonths 40%
-- 3. deepCount 15%
-- 4 clusterConsequtive 25%
--

aa = [6,5,1,6,4,2,2,3,7,2,1,1,2,3,4,5,6,7]
bb = "CptS322 - CptS322 - CptS 321"
cc = [[1,2],[1],[],[3],[1],[]]
dd = [('a',1),('a',2),('b',1),('a',2),('c',1),('b',1),('a',2)]
ee = ["Let","it","snow","let","it","rain","let","it","hail"]
-- 1. getUniqueRight: takes a list as input and returns the unique values from the list.
--      My function should keeps the last (rightmost) copies of the duplicate elements in the result.
--      The function should not sort the elements in the input list.
--      May use elem function.
-- As always, what does the function do?
--  [6,5,1,6,4,2,2,3,7,2,1,1,2,3,4,5,6,7] -> [1,2,3,4,6,7]
--  From this behavior, the function will go through each element in the original list compare it with
--  those in the buffer, if it exists in buffer, it does not get added.
--  Elem a [cbra] => True
--
--      1. Step 1:
--          gur il = helper il buf
--              where
--                  helper [] buf = buf
--                  helper (x:xs) buf | (elem x buf \= True) = helper xs (x:xs)
--                                    | otherwise = helper xs buf
--
--      2. Step 2: true for N elements
--

getUniqueRight :: Eq a => [a] -> [a]
getUniqueRight iL = reverse (helper (reverse iL) [])
 where
  helper [] _ = [] 
  helper (x:xs) buf | (x `elem` buf) = helper xs buf
                    | otherwise = x : helper xs (x:buf)

-- Type signature:
getUniqueLeft :: Eq a => [a] -> [a]
getUniqueLeft iL = helper iL []
 where
  helper [] buf = reverse buf
  helper (x:xs) buf | ((x `elem` buf) == True) = helper xs buf
                    | otherwise = helper xs (x:buf)


