{-
Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
-}

bound = 2*10^6

fact n= product [1..n]

toDigits :: Integer -> [Integer]
toDigits x | x >= 10   = (x `mod` 10) : toDigits (x `div` 10)
           | otherwise = [x]

check x = x == (sum $ map fact (toDigits x))

main = print $ filter check [3..bound]