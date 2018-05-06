-- https://www.hackerrank.com/contests/projecteuler/challenges/euler206
-- https://projecteuler.net/problem=206

main = do
    s <- getContents
    let [_, pattern] = lines s
    putStrLn $ show $ calc pattern

test :: String -> Integer -> Bool
test patternNoGaps x = (==) patternNoGaps $ dropEverySecond $ show $ x * x
   
dropEverySecond :: String -> String
dropEverySecond [] = []
dropEverySecond [x] = [x]
dropEverySecond (x:y:xs) = x:(dropEverySecond xs) 

replaceGaps a = map (\x -> if x==' ' then a else x)

calc ::  String -> Integer 
calc pattern = head $ filter f list 
    where 
          offsets = offsetsByPatternDigit lastPatternDigit

          lowerEndingWith0 :: Integer
          lowerEndingWith0 = (* 10) $ floor $ (/10) $ sqrt $ fromIntegral $ read patternFilledWith0s
          
          upper = floor $ sqrt $ fromIntegral $ read patternFilledWith9s 

          patternFilledWith0s = replaceGaps '0' pattern
          patternFilledWith9s = replaceGaps '9' pattern

          list :: [Integer]
          list = concat $ map generateRange $ map (\x -> lowerEndingWith0 + x) offsets

          lastPatternDigit = read $ [last pattern]
          patternNoGaps = dropEverySecond pattern 

          f = test patternNoGaps

          generateRange lower = [lower, (lower + 10)..upper] 

offsetsByPatternDigit :: Integer -> [Integer]
offsetsByPatternDigit 0 = [0]
offsetsByPatternDigit 1 = [1,9]
offsetsByPatternDigit 4 = [2,8]
offsetsByPatternDigit 9 = [3,7]
offsetsByPatternDigit 6 = [4,6] 
offsetsByPatternDigit 5 = [5]

