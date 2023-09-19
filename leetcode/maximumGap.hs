-- https://leetcode.com/problems/maximum-gap/description/

import Data.List

maxgap :: [Int] -> Int -> Int
maxgap [] _ = 0
maxgap [x] k = k
maxgap (x : xs) gap = max currentGap (maxgap xs currentGap)
  where
    currentGap = head xs - x

solve :: [Int] -> Int
solve arr = if length arr > 2 then maxgap (sort arr) 0 else 0
