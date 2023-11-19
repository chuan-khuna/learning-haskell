-- https://leetcode.com/problems/reduction-operations-to-make-the-array-elements-equal/description/

import Data.List as List
import Data.Map as Map

valueCount :: [Int] -> Map.Map Int Int
valueCount [] = Map.empty
valueCount arr = valueCount' arr Map.empty
  where
    valueCount' [] m = m
    valueCount' (x : xs) m = valueCount' xs (Map.insertWith (+) x 1 m)

reductionOperationCount :: [Int] -> Int
reductionOperationCount arr = numOperation
  where
    cumulativeOpt :: [Int] -> [Int]
    -- cumulative operation at index i is the sum of all elements from index 0 to i
    cumulativeOpt [] = []
    cumulativeOpt (x : xs) = x : Prelude.map (+ x) (cumulativeOpt xs)

    uniqueValues = nub $ reverse $ List.sort arr
    valuesToBeDecreased = List.delete (minimum uniqueValues) uniqueValues
    valueCountMap = valueCount arr
    counts = Prelude.map (\val -> Map.findWithDefault 0 val valueCountMap) valuesToBeDecreased
    numOperation = sum (cumulativeOpt counts)


reductionOperationCount [5,1,3]