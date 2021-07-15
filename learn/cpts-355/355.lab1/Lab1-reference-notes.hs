copyList :: [a] -> [a]
copyList [] = []
copyList (x : xs) = x : (copyList xs)

nthElement [] n = error "nthElement': The iput list is too short."
nthElement (x : xs) 1 = x
nthElement (x : xs) n = (nthElement xs (n -1))

-- insert 3 10 [1,2,3,10,4,5,6,7,8]
-- insert 0 10 []               --> [10]
-- insert 3 10 []               --> [] because the index is larger than the length of the list
-- insert 0 10 (x:xs)
-- insert 0 10 [4,5,6,7,8]      --> 10:4:[4,5,6,7,8]
--              ^
--              |
--              this means list is NOT empty
--
-- insert 3 10 [4,5,6,7,8]      --> 4:(insert (3-1) 10:[4,5,6,7,8])

-- insert n item [] | (n==0) = item:[]
--                  | otherwise = []
-- insert n item (x:xs) | (n==0) = item:x:xs
--                      | otherwise = x:(insert (n-1) item xs)

insert :: (Eq t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
insert 0 item [] = item : []
insert n item [] = []
insert n item (x : xs)
  | (n == 0) = item : x : xs
  | otherwise = x : (insert (n -1) item xs)

-- n > 0
-- case to consider
-- original call insertEvery 3 10 [1,2,3,4,5,6,7,8] --> [1,2,3,10,4,5,6,10,7,8]

-- insert 3 10 [1,2,3,10,4,5,6,10,7,8]
-- insertEvery m 0 10 []               --> [10]
-- insertEvery m 3 10 []               --> []
-- insertEvery m 0 10 [4,5,6,7,8,9,11] --> [4,5,6,10,7,8,9,10,11] <-- 10:4:(insertEvery m m 10 [7,8,9,11])
-- insertEvery m 3 10 [4,5,6,7,8,9,11] --> 4:(insertEvery m (3-1) 10 [7,8,9,11])

-- insertEvery m 0 item [] = item:[]
-- insertEvery m n item [] = []
-- insertEvery m n item [] | (n==0) = item:[]
--                         | otherwise = []
-- insertEvery m n item (x:xs) | (n==0) = item:x:(insertEvery m (m-1) item xs)
--                             | otherwise = x:(insertEvery m (n-1) item xs)

helper m n item []
  | (n == 0) = item : []
  | otherwise = []
helper m n item (x : xs)
  | (n == 0) = item : x : (helper m (m -1) item xs)
  | otherwise = x : (helper m (n -1) item xs)

insertEvery n item iL = helper n n item iL
  where
    helper m n item []
      | (n == 0) = item : []
      | otherwise = []
    helper m n item (x : xs)
      | (n == 0) = item : x : (helper m (m -1) item xs)
      | otherwise = x : (helper m (n -1) item xs)

-- getSales
-- storelog = [("Mon", 50), ("Fri", 20), ("Tue", 20)]
-- getSales "Fri" storelog
--

-- getSeconds [] = []
-- getSeconds (x:xs) = (snd x) : (getSeconds xs) -- this will recursively return second elements
-- however we can write it this way, without using the function snd(which returns the second element):
-- getSeconds ((x,y):xs) = y : (getSeconds xs)
-- if we want to add all y up:
-- getSecondsSum [] = 0
-- getSecondsSum ((x,y):xs) = y + (getSecondsSum xs)
--
getSecondsSum d [] = 0
getSecondsSum d ((x, y) : xs) = y + (getSecondsSum d xs)

sales =
  [ ("Amazon", [("Mon", 30), ("Wed", 100), ("Sat", 200)]),
    ("Etsy", [("Mon", 50), ("Tue", 20), ("Wed", 25), ("Fri", 30)]),
    ("Ebay", [("Tue", 60), ("Wed", 100), ("Thu", 30)]),
    ("Etsy", [("Tue", 100), ("Thu", 50), ("Sat", 20), ("Tue", 10)])
  ]

-- sumSales s day [] = 0
-- sumSales s day ((store, log) : xs)  | (s==store) = sumSalesHelper day log
--                                     | where sumSalesHelper day ((x,y) : ys) = y + (sumSalesHelper day ys)
--                                     | othewise = sumSales s day xs
--

splitHelper v [] buf = (reverse buf) : []
splitHelper v (x : xs) buf = splitHelper v xs (x : buf)

-- write a function "groupby3" that takes a list as inp8ut and splits the list
-- into groups of "3" elements. The leftovers are included as the last group.
--
-- Examples
-- g1 = groupby3 [1,2,3,4,5,6,7,8,9,10]     --[ [1,2,3], [4,5,6], [7,8,9], [10] ]
--
-- list =[1,2,3,4,5,6,7,8,9,10]
-- 1:[2,3,4,5,6,7,8,9,10] []  -> helper [2,3,4,5,6,7,8,9,10] [1] --> helper [3,4,5,6,7,8,9,10] [2,1] --> helper [4,5,6,7,8,9,10] [3,2,1]
-- helper 4:[5,6,7,8,9,10] [3,2,1] --> helper [3,2,1]:(helper 5:[6,7,8,9,10] [4]) --> [3,2,1]:(helper [6,7,8,9,10] [5,4])
-- --> helper [3,2,1]:(helper 6:[7,8,9,10] [5,4]) --> [3,2,1]:(helper [7,8,9,10] [6,5,4])
-- --> [6,5,4]:[3,2,1]:(helper [8,9,10] [7])

groupHelper [] buf = (reverse buf) : []
groupHelper (x : xs) buf
  | (length buf) < 3 = groupHelper xs (x : buf) -- the case that we have room in buf
  | otherwise = (reverse buf) : (groupHelper xs (x : []))

split' v iL = splitHelper v iL []
  where
    splitHelper2 v [] buf = (reverse buf) : []
    splitHelper2 v (x : xs) buf
      | (x /= v) = splitHelper2 v xs (x : buf) -- the case that we have room in buf
      | otherwise = (reverse buf) : (splitHelper2 v xs ([])) -- we pass list without x as in (x:[]) to exlucde the value from the list

-- [] (n > 0)                   = []
-- (x:xs) (x == v) and (n == 0) = reverse buf : []      -- we should NOT do any more splits
-- (x:xs) (x == v) and (n > 0)  =                       -- continue doing the splits; decrement n
-- (x:xs) (x /= v) (n)          =                       -- recurse for the rest of the list

nSplit' v n iL = splitHelper3 v n iL []
  where
    splitHelper3 v n [] buf
      | (buf == []) = []
      | otherwise = (reverse buf) : []
    splitHelper3 v n (x : xs) buf
      | (x == v) && (n == 0) = (x : xs) : [] -- not make the sploit, keep the rest of the list as it
      | (x == v) && (n > 0) = (reverse buf) : (splitHelper3 v (n -1) xs []) -- the case that we have room in buf
      | (x /= v) && (n == 0) = (x : xs) : [] -- we pass list without x as in (x:[]) to exlucde the value from the list
      | (x /= v) && (n > 0) = splitHelper3 v n xs (x : buf)