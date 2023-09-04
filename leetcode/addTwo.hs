-- https://leetcode.com/problems/add-two-numbers/
-- 2. Add Two Numbers

addTwo :: [Int] -> [Int] -> Int -> [Int]
addTwo [] [] 0 = []
addTwo [] [] 1 = [1]
addTwo (x : xs) [] c = addTwo (x : xs) [0] c
addTwo [] (y : ys) c = addTwo [0] (y : ys) c
addTwo (x : xs) (y : ys) c = reverse (((x + y + c) `mod` 10) : reverse (addTwo xs ys k))
  where
    k = (x + y + c) `div` 10

-- readable format
-- val1 = [1, 2, 3]
-- val2 = [1, 2, 3]

-- problem input
-- val1_in = reverse val1
-- val2_in = reverse val2

-- addTwo val1_in val2_in 0
