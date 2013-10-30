{-
F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8

What is the first term in the Fibonacci sequence to contain 1000 digits?
-}

first (x, _) = x

pair x y = (x, y)

main = print $ first $ head $ dropWhile (\(_, fn) -> length (show fn) < 1000) $ zipWith pair [1..] fibs

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
