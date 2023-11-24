import qualified Data.List as List

solve :: [Int] -> Int
solve piles = sum $ takeEven remainingPiles
  where
    sortedPiles = List.sort piles
    -- remove bob's piles
    remainingPiles = drop (length sortedPiles `div` 3) sortedPiles

    takeEven :: [Int] -> [Int]
    -- take the even indexed (0, 2, ...) piles
    takeEven [] = []
    takeEven (x : xs) = x : takeEven (drop 1 xs)
