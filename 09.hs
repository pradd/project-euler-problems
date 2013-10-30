{-
A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

a^2 + b^2 = c^2

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

main = print $ map product candidates

candidates = [[a, b, sqr (a^2 + b^2)]| a<-[1..999], b<-[1..a], a + b + (sqr (a^2 + b^2)) == 1000, sqrtable (a^2 + b^2)]

sqr :: Integer -> Integer
sqr x = toInteger $ floor $ sqrt $ fromIntegral x

sqrtable x = (sqrt $ fromIntegral x) == (fromIntegral $ sqr $ fromIntegral x) 