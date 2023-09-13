-- extend :: [Int] -> Int -> [[Int]]
-- extend [] k = [[]]
-- extend [x] k
--     | (x - 2 * k > 0) && (x - 2 * k) > k = ([k] ++ [x - 2 * k] ++ [k]) : extend [x] (succ k)
--     | otherwise = [[x]]

-- extend side
-- prop edge to edge
prop :: [Int] -> Int -> [[Int]]
prop [] k = [[]]
prop [x] k
    | (x - 2 * k > 0) && (x - 2 * k) > k = [result] ++ resultRec ++ prop [x] (succ k)
    | otherwise = [[x]]
  where
    result = [k] ++ [x - 2 * k] ++ [k]
    -- recursive call to the result
    resultRec = filter (not . null) (prop result 1)
prop (x : xs) k
    | head arr - k > k = [result] ++ resultRec ++ prop arr (succ k)
    | otherwise = [[]]
  where
    arr = x : xs
    result = [k] ++ [head arr - k] ++ init (tail arr) ++ [last arr - k] ++ [k]
    resultRec = filter (not . null) (prop result 1)

-- filter (not . null) (concatMap (`prop` 1) (prop [10] 1))

splitList arr
    -- check if odd
    | length arr `mod` 2 == 1 = [a, [arr !! half], d]
    | otherwise = [a, b]
  where
    half = length arr `div` 2
    -- half for even
    (a, b) = splitAt half arr
    -- (a, d)  for odd
    (c, d) = splitAt (half + 1) arr
