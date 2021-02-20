--
-- 2. cansInLog and numCans and getMonths
--
feedinglog = [("Oceanfish", 7), ("Tuna", 1), ("Whitefish", 3), ("Chicken", 4), ("Beef", 2)]

-- feedinglog is list of (flavor, num of cans)
--
-- as always, what does the function do?
-- GOAL: the function takes a month's feeding log as input and returns the total number of cans consumed.
-- OK, so it basically flatten the log into total number of cans, regardless of type.
--
--  1. step: the bas case
--      if there is one pair in the list, it adds to the accumulator
--      if there is no pair, it adds zero to the accumulator
--
--  2. this is true for N pairs in the list
--
cansInLog :: Num p => [(a, p)] -> p
cansInLog [] = 0
cansInLog ((x, y) : xs) = (cansInLog xs) + y

myCatsLog =
  [ ((7, 2020), [("Oceanfish", 7), ("Tuna", 1), ("Whitefish", 3), ("Chicken", 4), ("Beef", 2)]),
    ((8, 2020), [("Oceanfish", 6), ("Tuna", 2), ("Whitefish", 1), ("Salmon", 3), ("Chicken", 6)]),
    ((9, 2020), [("Tuna", 3), ("Whitefish", 3), ("Salmon", 2), ("Chicken", 5), ("Beef", 2), ("Turkey", 1), ("Sardines", 1)]),
    ((10, 2020), [("Whitefish", 5), ("Sardines", 3), ("Chicken", 7), ("Beef", 3)]),
    ((11, 2020), [("Oceanfish", 3), ("Tuna", 2), ("Whitefish", 2), ("Salmon", 2), ("Chicken", 4), ("Beef", 2), ("Turkey", 1)]),
    ((12, 2020), [("Tuna", 2), ("Whitefish", 2), ("Salmon", 2), ("Chicken", 4), ("Beef", 2), ("Turkey", 4), ("Sardines", 1)]),
    ((1, 2021), [("Chicken", 7), ("Beef", 3), ("Turkey", 4), ("Whitefish", 1), ("Sardines", 2)])
  ]

-- numCans: take a yearly log and flatten it to the total number
-- 2 parts:
--      - operates on the monthly tuple items
--      - operates on the cans in that month ( call cansInLog)
--  1. step: base case
--      if there is an item in the list,
--          call cansInLog on that month
--      else if there is no more
--          return 0
--
--  2. step: recursive case: this is treu for N elements in the list
--
-- numCans also takes a year and only calculate the lists belong to that year

checkYear (x, y) year
  | y == year = True
  | y /= year = False

numCans [] _ = 0
numCans (x : xs) year = getCansHelper x xs year
  where
    getCansHelper ((t, v), ts) [] year
      | (v == year) = cansInLog ts
      | otherwise = 0
    getCansHelper ((x, y), list) (t : xs) year
      | (y == year) = (getCansHelper t xs year) + cansInLog list
      | otherwise = getCansHelper t xs year

-- getMonths, to check for diets
-- getMonths :: (Ord t1, Eq t2) => [(a,b), [(t2,t1)]] -> t1 -> t2 -> [(a,b)]
-- getMonths takes in Log, the number of cans, flavor
-- it will check each month for the flaor, if match, check for the number of cans

getMonths [] n f = []
getMonths (((m, y), ls) : xs) n f
  | checkValue f n ls False = (m, y) : getMonths xs n f
  | otherwise = getMonths xs n f
  where
    checkValue _ _ [] False = False
    checkValue _ _ _ True = True
    checkValue f n ((x, y) : xs) bool
      | (f == x) && (n < y) = True
      | (f == x) && (n >= y) = False
      | otherwise = checkValue f n xs False

checkValue _ _ [] False = False
checkValue _ _ _ True = True
checkValue f n ((x, y) : xs) bool
  | (f == x) && n < y = True
  | otherwise = checkValue f n xs bool