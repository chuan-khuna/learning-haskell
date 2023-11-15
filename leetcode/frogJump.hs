-- leetcode 403: https://leetcode.com/problems/frog-jump/description/

-- https://www.youtube.com/watch?v=fbpVsqtjkNE

-- assisted by Copilot
canCross :: [Int] -> Bool
canCross stones = solve stones 0 1
  where
    solve :: [Int] -> Int -> Int -> Bool
    -- solve stones value k
    -- check if `value` + k (this jump) can reach the last stone or in the `stones` list
    solve stones value k
        | currentValue == last stones = True
        | currentValue > last stones = False
        | currentValue `notElem` stones = False
        | otherwise = any (solve stones currentValue) (filter (> 0) [k - 1, k, k + 1])
      where
        -- jump to the next stone(or water?)
        currentValue = value + k
