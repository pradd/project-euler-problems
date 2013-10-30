{-
Find the sum of the digits in the number 100!

For example, 10! = 10 * 9 * ... * 3 * 2 * 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
-}

number = fact 100

fact n = product [1..n]

main = print $ sum $ map (\x -> read [x]) $ show number