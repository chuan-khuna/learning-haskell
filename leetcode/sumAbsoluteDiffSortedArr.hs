-- https://leetcode.com/problems/sum-of-absolute-differences-in-a-sorted-array/description/

-- https://www.youtube.com/watch?v=3nkc-e66JmA

solve :: [Int] -> [Int]
solve arr = solve' arr total leftSum index result
  where
    total = sum arr
    leftSum = 0
    index = 0
    result = []

    solve' :: [Int] -> Int -> Int -> Int -> [Int] -> [Int]
    -- nums, total, leftSum, index, result -> return result
    solve' [] _ _ _ result = result
    solve' (x : xs) total leftSum index result = solve' xs total (leftSum + x) (index + 1) (result ++ [diff])
      where
        -- right size: the number of elements to the right of x (not including x)
        right_size = length xs
        -- left size: the number of elements to the left of x (not including x)
        left_size = index
        right_sum = total - leftSum - x
        diff = ((x * left_size) - leftSum) + (right_sum - (x * right_size))
