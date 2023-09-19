-- https://leetcode.com/problems/maximum-gap/description/

import Data.List

maxGap :: [Int] -> Int -> Int
maxGap [] _ = 0
maxGap [x] k = k
maxGap (x : xs) gap = max currentGap (maxGap xs currentGap)
  where
    currentGap = head xs - x

solve :: [Int] -> Int
solve arr
    | length arr > 2 = maxGap (sort arr) 0
    | otherwise = 0
