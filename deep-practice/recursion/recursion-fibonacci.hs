-- In this session, we will try to figure out the Fibonacci sequence.
--
-- First, we must obtain the statement:
--          F(N) = F(N-2) + F(N-1), and
--          F(1) = 1 and
--          F(0) = 0
--
-- This means that the base case contains two values, and we need two buckets for recurring
-- of the separate calls F(N-2) and F(N-1).
--
-- Immediately, we can see that it's best that we break this into smaller scopes:
--
-- Fib(N) = fibo(N, prev, curr)
--
-- Fib N = fibo N 0 1
--  where
--      fibo 0 _ _ = curr
--      fibo N prev curr = fib N-1 curr prev+curr
--

-- fib i = fibon i 0 1
--     where
--         fibon i p c | i == 0 = 0
--                     | i == 1 = 1
--                     | otherwise = fibon (i-1) c p + fibon (i-2) p c

-- fib n = fibon n 0 1
--             where
--                 fibon i p c | i == 0 = c
--                             | otherwise = p+c + fibon (i-1) c p+c
--                             

fib n = fibon n 0 1 0
    where
        fibon 0 p c buf = buf
        fibon i p c buf = fibon (i-1) c (p+c) (p+c)
