-- yet another coding practice on the basics of Haskell

aa = [1, 2, 3]

bb = [4, 5, 6]

-- cpList, the simplest (unoptimized)
-- takes a list and transforms/morphs it into new list (returns the new list)
--      I.  base case
--      II. inductive case
-------------------------------------------------------------------------------------------------------
--  I.  The base case: what will the function do?
--      step 1:
--          input = [a,b] -> separated into a:[b],
--      step 2:
--          take a and marks it to be cons to the new list, while calling the next function (recursive)
--      step 3:
--          reach the base case, where the input list becomes empty,
--          then return an empty list,
--          then go back to each stack to cons to the returned list
--
--  II. The inductive case: this is true for N elements
cpList :: [a] -> [a]
cpList [] = []
cpList (x : xs) = x : (cpList xs)

cpList' :: [a] -> [a]
cpList' iL = helper iL []
  where
    helper :: [a] -> [a] -> [a]
    helper [] buf = reverse buf
    helper (x : xs) buf = helper xs (x : buf)

-- tailcpList, optimized to prevent dependence on stackframe
-- takes a list and transform it into an empty list while also populate another list with head element
-- pulled from the original list.
--  I.  The base case: what will the function do?
--      step 1:
--          input = [a,b] -> separated into a:[b] -> [b] AND a:[b]
--      step 2:
--          pop the previous stackframe since [b] is sufficient for the next call
--      step 3:
--          reach the base case, return the new list after cons b[a]
--
--  II. The inductive case: this is true for N elements
--tcpL :: [a] -> ([a] -> [a])
--tcpL [] cache = cache
--tcpL (x : xs) cache = tcpL xs (x : cache)

tailcpList :: [a] -> [a]
tailcpList iL = helper iL []
  where
    helper :: [a] -> [a] -> [a]
    helper [] buffer = buffer
    helper (x : xs) buffer = helper xs (x : buffer)

-- NOTE on the bahaviors of the cons operator:
-- On the left side: it separates the head from a list
-- On the right side: it connects the element as head of a list

-- find n'th element from a list
-- take a list, break it up from the head, one element at a time while also decrement the tracking index
-- if the tracking index reaches 0 (or 1?), that is the base case
--  I.  The base case: what will the function do?
--      step 1:
--          input = [a,b], n = 1 -> separate into a:[b] and decrement n -> [b], n - 1
--      step 2:
--          reach the base case, return a from a:[b]
--
--  II. The inductive case: this is  true for N elements
--n'thElem [] n = error "list is too short."
elemNth [] n = error "list too short."
elemNth (x : xs) 0 = x
elemNth (x : xs) n = (elemNth xs (n -1))

-- n'thElem 0 (x:xs) = x
-- n'thElem n (x:xs) = (n'thElem (n-1) xs)
