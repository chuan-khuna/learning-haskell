-- https://leetcode.com/problems/frequency-of-the-most-frequent-element/description/

import Data.List as List

increaseToElem :: [Int] -> Int -> Int -> [Int]
-- nums, target, k, result nums
increaseToElem [] _ _ = []
increaseToElem (x : xs) target k
    | x == target = x : increaseToElem xs target k
    -- be able to increase x, because the gap is smaller than k
    | (x < target) && (target - x <= k) = target : increaseToElem xs target (k - (target - x))
    | otherwise = x : xs

filterLessThanTarget :: [Int] -> Int -> [Int]
filterLessThanTarget arr target = filter (<= target) arr

solve :: [Int] -> Int -> Int
solve arr k = maxCount
  where
    sortedArr = reverse $ List.sort arr
    -- [[Int]], each [Int] is a filtered array, which is less than `target` (maximum of the array)
    -- then increase each element to target as many as possible base on `k`

    -- targets = map (\filteredArr -> increaseToElem filteredArr (maximum filteredArr) k) (map (filterLessThanTarget sortedArr) sortedArr)

    -- suggested via warning
    targets = map ((\filteredArr -> increaseToElem filteredArr (maximum filteredArr) k) . filterLessThanTarget sortedArr) sortedArr

    countTargets = map (\arr -> length $ filter (== maximum arr) arr) targets
    maxCount = maximum countTargets

solveOptimised :: [Int] -> Int -> Int
solveOptimised arr k = solveHelper (reverse (List.sort arr)) k 0
  where
    solveHelper :: [Int] -> Int -> Int -> Int
    -- arr, k, maxCount -> maxCount
    solveHelper [] _ maxCount = maxCount
    solveHelper (x : xs) k maxCount = solveHelper xs k newMaxCount
      where
        targetArr = increaseToElem (x : xs) x k
        count = length $ filter (== x) targetArr
        newMaxCount = max maxCount count
