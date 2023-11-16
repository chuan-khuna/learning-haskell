-- https://leetcode.com/problems/merge-k-sorted-lists/description/

merge :: [[Int]] -> [Int]
merge [] = []
merge (x : xs) = mergeTwo x (merge xs)
  where
    mergeTwo :: [Int] -> [Int] -> [Int]
    mergeTwo [] [] = []
    mergeTwo [] ys = ys
    mergeTwo xs [] = xs
    mergeTwo (x : xs) (y : ys)
        | x <= y = x : mergeTwo xs (y : ys)
        | otherwise = y : mergeTwo (x : xs) ys
