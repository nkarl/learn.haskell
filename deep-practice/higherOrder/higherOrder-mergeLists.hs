

-- Merge two lists
--
-- merge2 [3,2,1,6,5,4] [1,2,3] => [3,1,2,2,1,3,5,4]
--
-- the regular case:
--      merge2 (x:xs) (y:ys) buf = merge2 xs ys y:x:xs
--
-- the base cases:
--      merge2 (x:xs) [] buf = merge2 xs [] x:buf
--      merge2 [] (y:ys) buf = merge2 [] ys y:buf
--
-- complete:

aa = [3, 2, 1, 6, 5, 4]

bb = [1, 2, 3]

cc = []

merge2 :: [a] -> [a] -> [a]
merge2 xs [] = xs
merge2 [] ys = ys
merge2 (x:xs) (y:ys) = x : y : (merge2 xs ys)

-- merge2Tail :: [a] -> [a] -> [a]
merge2Tail l1 l2 = helper l1 l2 []
    where
     helper [] [] buf = buf
     helper (x:xs) [] buf =  helper xs [] (x:buf)
     helper [] (y:ys) buf =  helper ys [] (y:buf)
     helper (x:xs) (y:ys) buf = helper xs ys (x:y:buf)