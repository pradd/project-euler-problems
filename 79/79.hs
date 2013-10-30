{-
A common security method used for online banking is to ask the user for three random characters from a passcode. For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.

The text file, keylog.txt, contains fifty successful login attempts.

Given that the three characters are always asked for in order, analyse the file so as to determine the shortest possible secret passcode of unknown length.
-}

import Data.List

calc_phrase :: [String] -> Integer
calc_phrase tests = head [x | x<-[1..], all (is_applicable (show x)) tests]

is_applicable _          []            = True
is_applicable []          _            = False
is_applicable long@(l:ls) short@(s:ss) = if l == s then is_applicable ls ss
                                                   else is_applicable ls short

main = do file <- readFile "keylog2.txt"
          let simplifiedTests = nub $ dropFirstDups $ dropLastDups $ lines file
          print simplifiedTests
          print $ calc_phrase simplifiedTests
              
dropFirstDups tests = foldl [] f tests
    where f acc test@(x:xs) | any (is_similar test) acc = xs : acc
                            | otherwise                 = test : acc
                        
is_similar test x | test == x                   = False
                  | length x < 3                = False
                  | (take 2 test) == (take 2 x) = True
                  | otherwise                   = False
                  
dropLastDups tests = map reverse $ dropFirstDups $ map reverse tests