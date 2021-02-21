aa = [1, 2, 3, 5, 6, 7, 8, 9, 2, 3, 11, 12]

clusterConsecutive [] = []
clusterConsecutive (x : xs) = helper x xs [] []
  where
    helper x [] buf b = reverse (reverse (x : buf) : b)
    helper x (y : xs) buf b
      | x + 1 == y = helper y xs (x : buf) b
      | x + 1 < y || x > y = helper y xs [] (reverse (x : buf) : b)
      | otherwise = helper y xs buf b