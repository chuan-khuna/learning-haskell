-- https://leetcode.com/problems/n-queens/description/

cartProd :: Int -> [(Int, Int)]
cartProd n = [(x, y) | x <- [0 .. n - 1], y <- [0 .. n - 1]]

eliminate :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
eliminate coords (x, y) = filter (\(a, b) -> a /= x && b /= y && abs (a - x) /= abs (b - y)) coords

nQueens :: Int -> [[(Int, Int)]]
nQueens n = filter (\queens -> length queens == n) $ map (\queenCoord -> nQueens' (eliminate coords queenCoord) [queenCoord]) firstRowCoords
  where
    coords = cartProd n
    firstRowCoords = [(0, y) | y <- [0 .. n - 1]]
    -- coords, queens, return queen locations
    nQueens' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    nQueens' [] queens = queens
    nQueens' coords queens = nQueens' remainingCoords' queens'
      where
        (x, y) = head coords
        remainingCoords' = eliminate coords (x, y)
        queens' = (x, y) : queens
