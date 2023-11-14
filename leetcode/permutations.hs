-- leetcode 46: Permutations
-- leetcode 47: Permutations II

import Data.List

-- copilot generated answer
permute :: (Ord a) => [a] -> [[a]]
permute [] = [[]]
permute xs = [x : ys | x <- xs, ys <- permute (delete x xs)]

permuteII :: [Int] -> [[Int]]
permuteII [] = [[]]
permuteII xs = [x : ys | x <- nub xs, ys <- permute (delete x xs)]
