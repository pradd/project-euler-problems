{-
Find the number of integers 1 < n < 10^7, for which n and n + 1 have the same number of positive divisors. 

For example, 14 has the positive divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.
-}


{-
import qualified Data.Map as M

bound = 10^5 - 1

initialMap = M.fromAscList [(x, 1) | x<-[2..bound]]

divisorsMap = populateSieve initialMap 2

countPairs m = sum $ zipWith f m (tail m)
    where f (_, x) (_, y) | x == y    = 1
                          | otherwise = 0
                   
main = print $ countPairs $ M.toAscList divisorsMap
-}

import Control.Monad.ST
import Data.Array.ST

bound = 10^7

countPairs m = sum $ zipWith f m (tail m)
    where f x y | x == y    = 1
                | otherwise = 0

populateSieve m ix = do if ix > bound 
                        then return m
                        else do arr <- updateSieve m ix ix
                                arr2 <- populateSieve arr (ix+1)
                                return arr2 
                   
updateSieve m ix divisor = do if ix > bound 
                              then return m
                              else do   a <- readArray m ix
                                        writeArray m ix (a+1)
                                        arr <- updateSieve m (ix + divisor) divisor
                                        return arr 
                
run       = do  initialArray <- newArray (2,bound) 1 :: ST s (STArray s Int Int)
                arr <- populateSieve initialArray 2
                els <- getElems arr
                return (countPairs els)

main = print $ runST run
