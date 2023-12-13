-- https://leetcode.com/problems/special-positions-in-a-binary-matrix/

import qualified Data.List as List

getSpecialPositions :: [[Int]] -> [(Int, Int)]
getSpecialPositions mat = filter (\(i, j) -> mat !! i !! j == 1) $ filter (\(i, j) -> sumRow i == 1 && sumCol j == 1) cartesianCoords
  where
    cartesianCoords = [(i, j) | i <- [0 .. (length mat - 1)], j <- [0 .. length (head mat) - 1]]
    sumRow i = sum (mat !! i)
    sumCol j = sum (map (!! j) mat)

numSpecial :: [[Int]] -> Int
numSpecial mat = length $ getSpecialPositions mat
