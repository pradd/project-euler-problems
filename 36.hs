{-
The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)
-}

pal base x = (toDigits base x) == (reverse $ toDigits base x)

toDigits :: Integer -> Integer -> [Integer]
toDigits base x | x >= base = (x `mod` base) : toDigits base (x `div` base)
                | otherwise = [x]

main = print $ filter (\x -> (pal 10 x) && (pal 2 x)) [1..999999]