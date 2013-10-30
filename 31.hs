{-
In England the currency is made up of pound, ?, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, ?1 (100p) and ?2 (200p).

It is possible to make ?2 in the following way:

1?1 + 150p + 220p + 15p + 12p + 31p

How many different ways can ?2 be made using any number of coins?
-}


import Data.List

nominals = [200,100,50,20,10,5,2,1]

expand 0 _  = [[]]
expand remainder (n:noms) | remainder >= n && noms /= [] = (map (n:) (expand (remainder - n) (n:noms)) ) ++ (expand remainder noms)
                          | remainder >= n && noms == [] =  map (n:) (expand (remainder - n) (n:noms))
                          | otherwise                    =  expand remainder noms

calc n = length $ expand n nominals

main = print $ calc 200
