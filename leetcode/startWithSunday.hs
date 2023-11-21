-- project Euler 19

import Data.List as L
import Data.Map as M

daysOfWeek = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

idToDay :: Int -> String
idToDay id = daysOfWeek !! id

getDaysInMonth :: Int -> Int -> Int
getDaysInMonth year month
    | month == 2 = if isLeapYear year then 29 else 28
    | month `elem` [4, 6, 9, 11] = 30
    | otherwise = 31
  where
    isLeapYear year = year `mod` 4 == 0 && year `mod` 100 /= 0 || year `mod` 400 == 0

getPrevYearMonth :: Int -> Int -> (Int, Int)
getPrevYearMonth 1990 1 = (1990, 1)
getPrevYearMonth year month
    | month == 1 = (year - 1, 12)
    | otherwise = (year, month - 1)

getStartDayInMonth :: Int -> Int -> Int
-- getStartDayInMonth year month -> dayId
getStartDayInMonth 1990 1 = 0
getStartDayInMonth year month = day
  where
    (prevYear, prevMonth) = getPrevYearMonth year month
    startDayOfPrevMonth = getStartDayInMonth prevYear prevMonth
    daysInPrevMonth = getDaysInMonth prevYear prevMonth
    day = (startDayOfPrevMonth + daysInPrevMonth) `mod` 7

years = [1990 .. 2000]
months = [1 .. 12]

zipYearMonth :: [Int] -> [Int] -> [(Int, Int)]
zipYearMonth years months = [(year, month) | year <- years, month <- months]

startDays = [((year, month), idToDay (getStartDayInMonth year month)) | (year, month) <- zipYearMonth years months]

-- list of (year, month) that starts with Sunday
startWithSunday = [(x, y) | (x, y) <- startDays, y == "Sun"]
