-- https://leetcode.com/problems/missing-number/description/

missingNumber :: [Int] -> Int
-- arr [0..n] so the length of arr is n + 1
-- sum [1..n] is n(n+1)/2
missingNumber arr = length arr * (length arr + 1) `div` 2 - sum arr
