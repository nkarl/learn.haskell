-- yet another coding practice on the basics of Haskell

aa = [1,2,3]

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
cpList (x:xs) = x:(cpList xs)


-- tailcpList, optimized to avoid dependency on the previous stackframes
-- takes a list and transform it into an empty list while also populate another list with elements
-- pulled from the original list (pulled by the head).
--  I.  The base case: what will the function do?
--      step 1:
--          input = [a,b] -> separated into a:[b] -> [b] AND a:[b]
--      step 2:
--          pop the previous stackframe since [b] is sufficient for the next call
--      step 3:
--          reach the base case, return the new list after performing cons on b:[a]
--
--  II. The inductive case: this is true for N elements
tcpL :: [a] -> ([a] -> [a])
tcpL [] cache = cache
tcpL (x:xs) cache = tcpL xs (x:cache)

-- NOTE on the bahaviors of the cons operator:
-- On the left side: it separates the head from a list
-- On the right side: it connects the element as head of a list
-- 

