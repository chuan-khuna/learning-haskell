-- leetcode 46: Permutations
-- copilot generated answer

import Data.List

permute :: (Ord a) => [a] -> [[a]]
permute [] = [[]]
permute xs = [x : ys | x <- xs, ys <- permute (delete x xs)]
