
aa = [[1,2,3],[5,5],[4,5,6],[7,1,2,3,4,5],[5,6]]

deepCount :: (Num p, Eq t) => t -> [[t]] -> p
deepCount v iL = helper v iL 0
 where
  helper v [] n = n
  helper v (x:xs) n | (v `elem` x) = helper v xs (n+1)
                    | otherwise = helper v xs n

-- deepCount :: (Num p, Eq t) => t -> [[t]] -> p
-- deepCount v iL = helper v iL 0
--  where
--   helper v [] n = n
--   helper v (x:xs) n | (v `elem` x) = helper v xs (n+1)
--                     | otherwise = helper v xs n