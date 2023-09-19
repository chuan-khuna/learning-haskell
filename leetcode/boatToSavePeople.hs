-- https://leetcode.com/problems/boats-to-save-people/description/

import Data.List

pairPoeple :: [Int] -> Int -> [Int]
pairPoeple [] _ = []
pairPoeple [x] _ = [x]
pairPoeple (x : xs) limit
    | x + last xs <= limit = [x, last xs]
    | otherwise = pairPoeple (init (x : xs)) limit

solve :: [Int] -> Int -> [[Int]]
solve [] _ = []
solve [x] _ = [[x]]
solve (x : xs) limit
    | length (pairPoeple (x : xs) limit) == 2 = pairPoeple (x : xs) limit : solve remainingPeople limit
    | otherwise = [x] : solve xs limit
  where
    -- remove paired people from the list
    remainingPeople = delete (last xs) (delete x (x : xs))
