{-
Starting in the top left corner of a 2*2 grid, there are 6 routes 
(without backtracking) to the bottom right corner.
How many routes are there through a 20*20 grid?
-}

import Data.List

maxCoord = 20

pathsList = [(x, y, paths x y)| x <- [0..maxCoord], y <- [0..maxCoord]]

paths :: Int -> Int -> Integer 
paths x y | x == 0    = 1
          | y == 0    = 1
          | otherwise = let n = find' (x-1) y pathsList
                            m = find' x (y-1) pathsList
                        in n + m

find' x y ((a,b,n):ts) | x == a && y == b = n
                       | otherwise        = find' x y ts  
                    
unpack (_,_,n) = n
                        
main = print $ unpack $ last $ pathsList 