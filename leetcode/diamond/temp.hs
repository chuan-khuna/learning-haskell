-- extend :: [Int] -> Int -> [[Int]]
-- extend [] k = [[]]
-- extend [x] k
--     | (x - 2 * k > 0) && (x - 2 * k) > k = ([k] ++ [x - 2 * k] ++ [k]) : extend [x] (succ k)
--     | otherwise = [[x]]

f :: [Int] -> Int -> [[Int]]
f [] k = [[]]
f [x] k
    | (x - 2 * k > 0) && (x - 2 * k) > k = ([k] ++ [x - 2 * k] ++ [k]) : f [x] (succ k)
    | otherwise = [[x]]
f (x : xs) k
    | head arr - k > k = ([k] ++ [head arr - k] ++ init (tail arr) ++ [last arr - k] ++ [k]) : f arr (succ k)
    | otherwise = [[]]
  where
    arr = x : xs

filter (not . null) (concat (map (`f` 1) (f [10] 1)))

-- concatMap (`f` 1) (f [10] 1)
