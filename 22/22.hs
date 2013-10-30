{-
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file 
containing over five-thousand first names, begin by sorting it into alphabetical 
order. Then working out the alphabetical value for each name, multiply this value
by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, 
which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. 
So, COLIN would obtain a score of 938 * 53 = 49714.

What is the total of all the name scores in the file?
-}

import Data.List

chMap 'A' = 1 
chMap 'B' = 2
chMap 'C' = 3
chMap 'D' = 4
chMap 'E' = 5
chMap 'F' = 6
chMap 'G' = 7
chMap 'H' = 8
chMap 'I' = 9
chMap 'J' = 10
chMap 'K' = 11
chMap 'L' = 12
chMap 'M' = 13
chMap 'N' = 14
chMap 'O' = 15
chMap 'P' = 16
chMap 'Q' = 17
chMap 'R' = 18
chMap 'S' = 19
chMap 'T' = 20
chMap 'U' = 21
chMap 'V' = 22
chMap 'W' = 23
chMap 'X' = 24
chMap 'Y' = 25
chMap 'Z' = 26

score :: [String] -> Integer
score file = foldl f 0 $ zip [1..] $ map calcScore $ sort file
    where f n (x, y) = n + x*y
          
calcScore :: String -> Integer
calcScore xs = foldl (\n x -> n+(chMap x)) 0 xs

main = do file <- readFile "names.txt"
          print $ score $ read file
          