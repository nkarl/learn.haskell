-- First Haskell source script
-- comment style is similar to Lua with the double hyphens
--
-- if a Haskell source script is to be compiled and executed, it needs a "main" function
-- to execute from. This is similar to the "main" file (which contains the main() function) in C++.
--
-- However, Haskell source script can also be loaded into the ghci and the functions in the source 
-- script can be be called directly.
--

doubleThis :: Int -> Int
doubleThis x = x + x

multiplyTwo :: Int -> Int -> Int
multiplyTwo x y = x * y


