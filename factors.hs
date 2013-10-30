module Factors (divisorsAll) where

--factors

--primeFactors

-- include 1 and n values
-- 28: [1,2,4,7,14,28]
divisorsAll :: Integer -> [Integer]
divisorsAll n = [x | x <- [1..(n `div` 2)], n `mod` x == 0] ++ [n]
