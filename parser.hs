
parse :: String -> [[Int]]
parse str = map lineToInts $ lines str

lineToInts :: String -> [Int]
lineToInts line = map read $ words line

main = do   str <- getContents
            print $ parse str