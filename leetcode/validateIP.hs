-- leetcode 468: Validate IP Address
-- https://leetcode.com/problems/validate-ip-address/description/

import Data.List
import qualified Data.Text as T

leadWithZero :: String -> Bool
leadWithZero s
    | length s > 1 && head s == '0' = True
    | otherwise = False

validateOctetValue :: String -> Bool
validateOctetValue s
    | num <= 255 && num >= 0 = True
    | otherwise = False
  where
    -- use condition to handle empty string
    num = if length s > 0 then read s :: Int else -1

validateOctet :: String -> Bool
validateOctet s = not (leadWithZero s) && validateOctetValue s

countDot :: String -> Int
countDot s = length (filter (== '.') s)

splitOctet :: String -> [String]
splitOctet s = map T.unpack (T.splitOn (T.pack ".") (T.pack s))

validateIPv4 :: String -> Bool
validateIPv4 s
    | countDot s /= 3 = False
    | otherwise = all validateOctet (splitOctet s)
