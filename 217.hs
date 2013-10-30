{-
A positive integer with k (decimal) digits is called balanced if its first k/2 digits sum to the same value as its last k/2 digits

So, for example, all palindromes are balanced, as is 13722.

Let T(n) be the sum of all balanced numbers less than 10^n. 
Thus: T(1) = 45, T(2) = 540 and T(5) = 334795890.

Find T(47) mod 3^15
-}

toDigits :: Integer -> [Integer]
toDigits x | x >= 10   = (x `mod` 10) : toDigits (x `div` 10)
           | otherwise = [x]

sumHalfDigits dig = sum $ take (k `div` 2) dig
    where k = length dig
           
balanced :: Integer -> Bool
balanced x = (sumHalfDigits dig) == (sumHalfDigits $ reverse dig)
    where dig = toDigits x  

tau n = sum $ filter balanced [1..10^n-1]

main = print $ tau 7