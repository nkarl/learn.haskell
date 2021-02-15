a = [0, 1, 2, 3, 4, 5, 6, 7, 8]

v = 10

pos = 3

step = 3

-- copy a list to a new list
copyList [] = []
copyList (x : xs) = x : (copyList xs)

-- find the nth element in the list
nthElem n [] = error "List is too short." -- if n > 1 but the list is already partitioned to empty
nthElem 1 (x : xs) = x -- the base case
nthElem n (x : xs) = (nthElem (n -1) xs)

-- insert an element into the list, BEFORE pos
-- example: insert 10 BEFORE pos 3 in the list a = [0,1,10,2,3,4,5,6,7,8]
insertBefore v 0 [] = v : [] -- if list is empty, just add
insertBefore v pos [] = error "Index is out of range of list." -- edge case
insertBefore v pos (x : xs)
  | (pos == 1) = v : (x : xs) -- if pos has decremented to 0, add
  | otherwise = x : (insertBefore v (pos -1) xs) -- otherwise, keep calling (even if pos < 0)

-- insert an element into the list, AT pos
-- example: insert 10 AT pos 3 in the list a = [0,1,2,10,3,4,5,6,7,8]
insert v 0 [] = v : [] -- if list is empty, just add
insert v pos [] = error "Index is out of range of list." -- edge case
insert v pos (x : xs)
  | (pos == 0) = v : (x : xs) -- if pos has decremented to 0, add
  | otherwise = x : (insert v (pos -1) xs) -- otherwise, keep calling

-- insert an element into the list, AFTER pos
-- example: insert 10 AFTER pos 3 in the list a = [0,1,10,2,3,4,5,6,7,8]
insertAfter v 0 [] = v : [] -- if list is empty, just add
insertAfter v pos [] = error "Index is out of range of list." -- edge case
insertAfter v pos (x : xs)
  | (pos == -1) = v : (x : xs) -- if pos has decremented to 0, add
  | otherwise = x : (insertAfter v (pos -1) xs) -- otherwise, keep calling (even if pos < 0)

-- insert an element into a list, but only at an interval of 3 elements
-- example: insert 10 at 3, 6, 9, etc.
-- we need a way to keep the original interval because pos will continue decrement after each call
insertEvery' v step pos [] -- case: empty list
  | (pos == 0) = v : []
  | otherwise = []
insertEvery' v step pos (x : xs) -- case: list not empty
  | (pos == 0) = v : x : (insertEvery' v step (step -1) xs)
  | otherwise = x : (insertEvery' v step (pos -1) xs)

insertEvery v step iL = helper v step step iL
  where
    helper v step pos [] -- case: empty list
      | (pos == 0) = v : []
      | otherwise = []
    helper v step pos (x : xs) -- case: list not empty
      | (pos == 0) = v : x : (helper v step (step -1) xs)
      | otherwise = x : (helper v step (pos -1) xs)