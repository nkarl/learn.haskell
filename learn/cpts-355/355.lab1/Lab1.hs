{-# LANGUAGE FlexibleContexts #-}

-- CptS 355 - Lab 1 (Haskell) - Spring 2021
-- Name: Charles Norden
-- Collaborated with:

module Lab1 where

-- 1.insert
insert :: (Eq t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
insert 0 item [] = item : []
insert n item [] = []
insert n item (x : xs)
  | (n == 0) = item : x : xs
  | otherwise = x : (insert (n -1) item xs)

-- 2. insertEvery
insertEvery :: (Eq t, Num t) => t -> a -> [a] -> [a]
insertEvery n item iL = helperInsEvery n n item iL
  where
    helperInsEvery m n item []
      | (n == 0) = item : []
      | otherwise = []
    helperInsEvery m n item (x : xs)
      | (n == 0) = item : x : (helperInsEvery m (m -1) item xs)
      | otherwise = x : (helperInsEvery m (n -1) item xs)

-- 3. getSales
getSales :: (Num p, Eq t) => t -> [(t, p)] -> p
getSales day [] = 0
getSales day ((x, y) : xs) = if day == x then y + getSales day xs else getSales day xs

-- 4. sumSales
sumSales :: (Num a, Eq t1, Eq t2) => t1 -> t2 -> [(t1, [(t2, a)])] -> a
sumSales s day [] = 0
sumSales s day ((store, log) : xs)
  | (s /= store) = sumSales s day xs
  | otherwise = sumHelper day log + (sumSales s day xs)
  where
    sumHelper day [] = 0
    sumHelper day ((x, y) : ys)
      | (day /= x) = sumHelper day ys
      | otherwise = y + sumHelper day ys

-- 5. split
split :: Eq a => a -> [a] -> [[a]]
split v iL = splitHelper v iL []
  where
    splitHelper v [] buf = [reverse buf]
    splitHelper v (x : xs) buf
      | (x /= v) = splitHelper v xs (x : buf) -- the case that we have room in buf
      | otherwise = reverse buf : splitHelper v xs [] -- we pass list without x as in (x:[]) to exlucde the value from the list

-- 6. nSplit
nSplit :: (Num a1, Ord a1, Eq a2) => a2 -> a1 -> [a2] -> [[a2]]
nSplit v n iL = nSplitHelper v n iL []
  where
    nSplitHelper v n [] buf
      | null buf = []
      | otherwise = [reverse buf]
    nSplitHelper v n (x : xs) buf
      | x == v && n == 0 = [x : xs] -- not make the sploit, keep the rest of the list as it
      | x == v && n > 0 = reverse buf : nSplitHelper v (n -1) xs [] -- the case that we have room in buf
      | x /= v && n == 0 = [x : xs] -- we pass list without x as in (x:[]) to exlucde the value from the list
      | x /= v && n > 0 = nSplitHelper v n xs (x : buf)