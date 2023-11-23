-- https://leetcode.com/problems/n-queens/description/

cartesianProduct :: Int -> [(Int, Int)]
cartesianProduct n = [(x, y) | x <- [0 .. n - 1], y <- [0 .. n - 1]]

eliminate :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
-- assisted by copilot
eliminate coords (x, y) = filter (\(a, b) -> a /= x && b /= y && abs (a - x) /= abs (b - y)) coords

nQueenSolve :: Int -> [[(Int, Int)]]
nQueenSolve n = filter (\solution -> length solution == n) $ nQueens' (cartesianProduct n) []
  where
    nQueens' :: [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
    -- remaining coords, queen coords -> all solutions
    nQueens' coords queens
        | null coords = [queens]
        | otherwise = concat [nQueens' (eliminate coords (x, y)) (queens ++ [(x, y)]) | (x, y) <- firstRowCoords]
      where
        firstRemainingRow = minimum [x | (x, y) <- coords]
        firstRowCoords = filter (\(x, y) -> x == firstRemainingRow) coords

n = 8
solutions = nQueenSolve n
length solutions
