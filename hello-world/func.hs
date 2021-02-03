-- main = putStrLn "Hello, world!"

mul x y = x * y

factorial n =
  if n > 1
    then n * factorial (n - 1)
    else 1

main = do
  putStrLn "What is 4 * 6?"
  x <- readLn
  if x == (mul 4 6)
    then putStrLn "You are right!"
    else putStrLn "You are wrong!"
