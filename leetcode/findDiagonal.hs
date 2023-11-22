-- https://leetcode.com/problems/diagonal-traverse-ii/description/

findDiagonal :: [[Int]] -> [Int]
findDiagonal nums = map (\(x, y) -> nums !! x !! y) diagonalCartProd
  where
    rowCount = length nums
    maxColCount = maximum $ map length nums
    -- cartesian product (row, col) coordinates
    cartProd = [(x, y) | x <- reverse [0 .. rowCount - 1], y <- [0 .. maxColCount - 1]]
    diagonalCartProd = concatMap (filter (\(x, y) -> y < length (nums !! x)) . (\n -> filter (\(x, y) -> x + y == n) cartProd)) [0 .. rowCount * maxColCount]
