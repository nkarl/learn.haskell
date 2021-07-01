-- PRACTICE ON MORE DIVERSIFIED IMPLEMENTATION OF RECURSIVE FUNCTIONS
--
-- In this session, I will apply what have been fleshed out in the basic implementation for the base and tail
-- optimization in a wider array of functions. These functions will perform more diverse tasks while serving
-- as a good examples for how we can use recursion in Haskell.
--
--
--------------------------------------------------------------------------------------------------------------
-- 1. inSert:
--------------------------------------------------------------------------------------------------------------
-- Write a function inSert that:
--   - takes an integer 'n', a value 'item' and a list 'iL', and then
--   - inserts the 'item' at index 'n' in the list 'iL'.
--
aa   = [1, 2, 3, 4, 5, 6]

n    = 3

item = 999

--------------------------------------------------------------------------------------------------------------
-- Other considerations:
--   - 'n' is a 1-based index, i.e. 'item' should be inserted after n'th element in the list.
--   - The type signature of inSert can be:
--
--           insert :: (Num t1) => t1 -> t2 -> [t2] -> [t2]
--
--     or,   insert :: (Eq t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
--
--     or,   insert :: (Ord t1, Num t1) => t1 - t2 -> [t2] -> [t2]
--
--------------------------------------------------------------------------------------------------------------
-- Let's not worry about the type signature for now. Let's think about the regular case and the recursive
-- case first, i.e. what we want the function to behave.
--
-- First, we implement the basic recursion:
--
-- Assume a list aa == [1,2,3,4,5], insert at n=3 the item=999 into the list aa.
--
-- In the regular case, that is the list is not empty, we have to:
--
--      - take a list, break it up recursively until
--      - we arrive at the correct index 'n', at which point
--      - we attach the 'item' to the list, and then
--      - attach the 'rest' to the new list.
--
-- We look through the steps above and see that we need (x:xs), a sentinel 'n' and an 'item'.
--
--          insert n item (x:xs) = x:(insert (n-1) item xs)
--
-- Eventually, we reach the base case. But we need to be careful here. What should be our base case?
-- Obviously we cannot continue passing the list until it is empty like in the cpString example. In such a
-- case, the sentinel 'n' will continue to be subtracted past zero and become negative.
--
-- Thus, our base case must be when 'n' reaches 0.
--
--          insert 0 item (x:xs) = xs
--          insert n item (x:xs) = x:(insert (n-1) item xs)
--
-- With the implementation above, we will get [1,2,3,5] as result because we forgot to attach item to the
-- 'rest'. Let's improve it:
--
--          insert 0 item (x:xs) = item:xs
--          insert n item (x:xs) = x:(insert (n-1) item xs)
--
-- With this improvement, we will get [1,2,3,999,5] as result, which means we have successfully inserted the
-- item=999 into a list. Although, this list not exactly the original list because it is missing the value 4.
-- Thus we must make another improvement:
--
--          insert 0 item (x:xs) = item:x:xs
--          insert n item (x:xs) = x:(insert (n-1) item xs)
--
-- Now, the function behaves correctly as we want it to!
--
--------------------------------------------------------------------------------------------------------------
-- Now let's go back to the prompt and look at a few important details.
--
-- 1. The index 'n' is 1-based. What does this mean? It means that the item should be inserted AFTER the n'th
--      element.
--
--          [1, 2, 3,  , 4, 5, 6]
--           ^  ^  ^  ^
--           |  |  |  |
--           3  2  1  0
--                    insert here
--
--
-- 2. Let's rewrite it with tail optimization. We obviously need a bucket to store the 'heads' and we also
--      need to remember to reverse the list once done. First we add a bucket:
--
--          insert 0 item (x:xs) bucket = item:x:bucket
--          insert n item (x:xs) bucket = insert (n-1) item xs (x:bucket)
--
--
--    With this implementation, we will get [4, 999, 3, 2, 1]. Notice that we are missing the 'rest' of the
--    original list. How do solve this problem?
--
--    First, let's move this new implementation into its own space (or scope) so that we keep the original
--    type signature without adding the bucket.
--
--          insert n item iL = let
--                               helper 0 item (x:xs) bucket = x:item:bucket
--                               helper n item (x:xs) bucket = helper (n-1) item xs (x:bucket)
--                             in
--                               insert n item iL = helper n item iL []
--
--
--   So, what is the issue here? Well, it is because we actually have 2 lists at this point. The first list is
--   the list correctly attached with the 'item', and the second list is the 'rest' of the original list that
--   we still need to iterate through and attach to the Bucket.
--
--   Thus, having reach the index n=0 is simply one of our base cases. Or to be precise, it is not yet the
--   real base case. It is merely an goalpost.
--
--   Therefore, we must add the real base case when n=0 and xs is devoid of elements:
--
--          insert n item iL = let
--                               helper 0 item []     bucket = bucket
--                               helper 0 item (x:xs) bucket = helper 0 item xs (x:item:bucket)
--                               helper n item (x:xs) bucket = helper (n-1) item xs (x:bucket)
--                             in
--                               insert n item iL = helper n item iL []

insert n item iL =
    let
      helper 0 item []     bucket = bucket
      helper 0 item (x:xs) bucket = helper 0 item xs (x:item:bucket)
      helper n item (x:xs) bucket = helper (n-1) item xs (x:bucket)
     in
      helper n item iL []
