{-
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
-}

import Data.List
import Data.Time

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving ( Eq, Show )

seed  = fromGregorian 1900 1  1
start = fromGregorian 1901 1  1
end   = fromGregorian 2000 12 31 

doeCycle :: [DayOfWeek]
doeCycle = Mon:Tue:Wed:Thu:Fri:Sat:Sun:doeCycle

predicate :: (DayOfWeek, Data.Time.Day) -> Bool
predicate (x, y) = x == Sun && day == 1
     where (_,_,day) = toGregorian y

main = print $ length $ filter predicate $ dropWhile (\(_,x) -> x /= start) $ zip doeCycle [seed .. end] 


{-
isLeapYear y | y `mod` 400 == 0 = True
             | y `mod` 100 == 0 = False
             | y `mod` 4   == 0 = True
             | otherwise        = False
              
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq, Ord, Enum, Show)
data Year  = Year Int deriving Eq
data Day   = Day Int deriving Eq

data Date  = Date Year Month Day deriving Eq

instance Enum Date where
    succ (Date (Year year) month (Day day)) | day == (daysInMonth month) = id

nextMonth mon = snd $ (\ (Just x) -> x) $ find predicate transitions 
        where transitions = (Dec, Jan):(zip [Jan .. Nov] [Feb .. Dec])
              predicate (x, y) = mon == x 

start = Date (Year 1901) Jan (Day 1)
end   = Date (Year 2000) Dec (Day 31)

daysInMonth Feb year | isLeapYear year = 29
                     | otherwise       = 28
daysInMonth mon _ = if mon `elem` [Sep, Apr, Jun, Nov] then 30
                                                       else 31
-}
